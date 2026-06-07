mod js_platform;
pub mod workspace;

use argon_testcases::TestCase;
use hashbrown::{HashMap, HashSet};
pub use js_platform::*;
use serde::Deserialize;
use std::ffi::OsStr;
use std::path::PathBuf;
use std::process::Command;
use std::sync::{Arc, Mutex};
use tempfile::TempDir;

pub struct TestSuiteContext<P: CompileTargetPlatform> {
    pub platform: Arc<P>,
    pub test_suite_data_dir: TempDir,
    pub lib_dir: PathBuf,
    pub backend_dir: PathBuf,
    pub argon_bin: PathBuf,
    pub print_commands: bool,

    pub library_platform_metadata: Mutex<HashMap<String, Vec<PathBuf>>>,
    pub library_compiled_tubes: Mutex<HashMap<String, PathBuf>>,
    pub library_generated_ir: Mutex<HashMap<String, PathBuf>>,
    pub platform_state: P::PlatformState,
}

impl<P: CompileTargetPlatform> TestSuiteContext<P> {
    pub fn library_info(self: Arc<Self>, library_name: &str) -> LibraryInfo<P> {
        let library_path = self.lib_dir.join(library_name);
        let library_output_path = self
            .test_suite_data_dir
            .path()
            .join("lib")
            .join(library_name);

        std::fs::create_dir_all(&library_output_path).unwrap();

        LibraryInfo {
            name: library_name.to_owned(),
            test_suite_context: self,
            library_path,
            library_output_path,
        }
    }

    pub fn build_library_metadata(self: Arc<Self>, library_name: &str) -> Vec<PathBuf> {
        let mut metadata_map = self
            .library_platform_metadata
            .lock()
            .unwrap_or_else(|e| e.into_inner());

        let context2 = self.clone();

        metadata_map
            .entry_ref(library_name)
            .or_insert_with(|| {
                context2
                    .platform
                    .clone()
                    .library_platform_metadata(&context2.library_info(library_name))
            })
            .clone()
    }

    pub fn library_dependencies(&self, library_name: &str) -> Vec<String> {
        let manifest_path = self.lib_dir.join(library_name).join("lib.toml");
        if !manifest_path.exists() {
            return Vec::new();
        }

        let manifest = std::fs::read_to_string(&manifest_path).unwrap_or_else(|err| {
            panic!(
                "Reading library manifest {} failed: {}",
                manifest_path.display(),
                err
            )
        });
        let manifest =
            toml_edit::de::from_str::<LibraryManifest>(&manifest).unwrap_or_else(|err| {
                panic!(
                    "Decoding library manifest {} failed: {}",
                    manifest_path.display(),
                    err
                )
            });

        manifest.dependencies.libraries
    }

    pub fn referenced_libraries(self: &Arc<Self>, libraries: &[String]) -> Vec<String> {
        let mut referenced_libraries = Vec::new();
        let mut seen = HashSet::new();

        self.collect_referenced_library("Argon.Core", &mut seen, &mut referenced_libraries);
        for library_name in libraries {
            self.collect_referenced_library(library_name, &mut seen, &mut referenced_libraries);
        }

        referenced_libraries
    }

    fn collect_referenced_library(
        self: &Arc<Self>,
        library_name: &str,
        seen: &mut HashSet<String>,
        referenced_libraries: &mut Vec<String>,
    ) {
        if !seen.insert(library_name.to_owned()) {
            return;
        }

        for dependency_name in self.library_dependencies(library_name) {
            self.collect_referenced_library(&dependency_name, seen, referenced_libraries);
        }

        referenced_libraries.push(library_name.to_owned());
    }

    pub fn print_command(&self, cmd: &Command) {
        if self.print_commands {
            println!("$ {}", format_command(cmd));
        }
    }

    pub fn compile_library_tube(self: Arc<Self>, library_name: &str) -> PathBuf {
        if let Some(path) = self
            .library_compiled_tubes
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .get(library_name)
            .cloned()
        {
            return path;
        }

        let dependency_references = self
            .referenced_libraries(&self.library_dependencies(library_name))
            .into_iter()
            .filter(|dependency_name| dependency_name != library_name)
            .map(|dependency_name| self.clone().compile_library_tube(&dependency_name))
            .collect::<Vec<_>>();

        let mut tube_map = self
            .library_compiled_tubes
            .lock()
            .unwrap_or_else(|e| e.into_inner());

        if let Some(path) = tube_map.get(library_name).cloned() {
            return path;
        }

        let context2 = self.clone();
        let library = context2.library_info(library_name);

        let output_file = library
            .library_output_path
            .join(format!("{}.artube", library.name));

        let mut cmd = Command::new(&library.test_suite_context.argon_bin);
        cmd.arg("compile");
        cmd.arg("--tube-name");
        cmd.arg(&library.name);

        cmd.arg("--input");
        cmd.arg(library.library_path.join("src"));

        cmd.arg("--output");
        cmd.arg(&output_file);

        let metadata_files = library
            .test_suite_context
            .clone()
            .build_library_metadata(library_name);

        for metadata_file in metadata_files {
            cmd.arg("--platform");
            cmd.arg(metadata_file);
        }

        for dependency_reference in dependency_references {
            cmd.arg("--reference");
            cmd.arg(dependency_reference);
        }

        library.test_suite_context.print_command(&cmd);
        let output = cmd.output().unwrap();

        assert!(
            output.status.success(),
            "Compilation of library {} failed\n{}\n{}",
            library.name,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );

        tube_map.insert(library_name.to_owned(), output_file.clone());

        output_file
    }

    pub fn genir_library_tube(self: Arc<Self>, library_name: &str) -> PathBuf {
        let mut tube_map = self
            .library_generated_ir
            .lock()
            .unwrap_or_else(|e| e.into_inner());

        let context2 = self.clone();

        tube_map
            .entry_ref(library_name)
            .or_insert_with(|| {
                let library = context2.library_info(library_name);

                let input_file = library
                    .test_suite_context
                    .clone()
                    .compile_library_tube(library_name);
                let output_file = library
                    .library_output_path
                    .join(format!("{}.arvm", library.name));

                let dependency_references = library
                    .test_suite_context
                    .referenced_libraries(
                        &library
                            .test_suite_context
                            .library_dependencies(library_name),
                    )
                    .into_iter()
                    .filter(|dependency_name| dependency_name != library_name)
                    .map(|dependency_name| {
                        library
                            .test_suite_context
                            .clone()
                            .compile_library_tube(&dependency_name)
                    })
                    .collect::<Vec<_>>();

                let mut cmd = Command::new(&library.test_suite_context.argon_bin);
                cmd.arg("genir");

                cmd.arg("--input");
                cmd.arg(input_file);

                cmd.arg("--platform");
                cmd.arg(P::ID);

                cmd.arg("--output");
                cmd.arg(&output_file);

                for dependency_reference in dependency_references {
                    cmd.arg("--reference");
                    cmd.arg(dependency_reference);
                }

                library.test_suite_context.print_command(&cmd);
                let output = cmd.output().unwrap();

                assert!(
                    output.status.success(),
                    "IR Generation of library {} failed\n{}\n{}",
                    library.name,
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                );

                output_file
            })
            .clone()
    }
}

#[derive(Deserialize, Default)]
#[serde(deny_unknown_fields)]
struct LibraryManifest {
    #[serde(default)]
    dependencies: LibraryManifestDependencies,
}

#[derive(Deserialize, Default)]
#[serde(deny_unknown_fields)]
struct LibraryManifestDependencies {
    #[serde(default)]
    libraries: Vec<String>,
}

pub struct TestContext<P: CompileTargetPlatform> {
    pub test_suite_context: Arc<TestSuiteContext<P>>,
    pub test_data_dir: PathBuf,
    pub test_case: Arc<(String, TestCase)>,
}

impl<P: CompileTargetPlatform> TestContext<P> {
    pub fn referenced_libraries(&self) -> Vec<String> {
        self.test_suite_context
            .referenced_libraries(&self.test_case.1.libraries)
    }

    pub fn compile_test_case(&self) -> Result<PathBuf, String> {
        let output_file = self.test_data_dir.join("Argon.TestCase.artube");
        if output_file.exists() {
            return Ok(output_file);
        }

        let source_dir = self.test_data_dir.join("src");

        std::fs::create_dir_all(&source_dir).unwrap();

        for f in &self.test_case.1.input_sources {
            let source_path = source_dir.join(&f.name);
            std::fs::write(source_path, &f.source).unwrap();
        }

        let mut cmd = Command::new(&self.test_suite_context.argon_bin);
        cmd.arg("compile");
        cmd.arg("--tube-name");
        cmd.arg("Argon.TestCase");
        cmd.arg("--input");
        cmd.arg(source_dir);
        cmd.arg("--output");
        cmd.arg(&output_file);

        for library_name in self.referenced_libraries() {
            let tube_path = self
                .test_suite_context
                .clone()
                .compile_library_tube(&library_name);
            cmd.arg("--reference");
            cmd.arg(tube_path);
        }

        self.test_suite_context.print_command(&cmd);
        let output = cmd.output().unwrap();

        if output.status.success() {
            Ok(output_file)
        } else {
            Err(format!(
                "{}\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ))
        }
    }

    pub fn genir_test_case(&self) -> PathBuf {
        let output_file = self.test_data_dir.join("Argon.TestCase.arvm");
        let input_file = self.compile_test_case().unwrap();

        let mut cmd = Command::new(&self.test_suite_context.argon_bin);
        cmd.arg("genir");
        cmd.arg("--input");
        cmd.arg(input_file);

        cmd.arg("--platform");
        cmd.arg(P::ID);

        cmd.arg("--output");
        cmd.arg(&output_file);

        for library_name in self.referenced_libraries() {
            let tube_path = self
                .test_suite_context
                .clone()
                .compile_library_tube(&library_name);
            cmd.arg("--reference");
            cmd.arg(tube_path);
        }

        self.test_suite_context.print_command(&cmd);
        let output = cmd.output().unwrap();

        assert!(
            output.status.success(),
            "IR generation of test case failed\n{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );

        output_file
    }
}

pub struct LibraryInfo<P: CompileTargetPlatform> {
    pub name: String,
    pub test_suite_context: Arc<TestSuiteContext<P>>,
    pub library_path: PathBuf,
    pub library_output_path: PathBuf,
}

pub trait CompileTargetPlatform: Sized + Send + Sync + 'static {
    type PlatformState: Default + Send + Sync + 'static;

    const ID: &'static str;

    fn library_platform_metadata(self: Arc<Self>, library: &LibraryInfo<Self>) -> Vec<PathBuf>;
    fn codegen(self: Arc<Self>, test_context: &TestContext<Self>);
    fn run(self: Arc<Self>, test_context: &TestContext<Self>) -> TestExecutionResult;
}

pub struct TestExecutionResult {
    pub exit_code: i32,
    pub output: String,
}

fn format_command(cmd: &Command) -> String {
    std::iter::once(cmd.get_program())
        .chain(cmd.get_args())
        .map(quote_arg)
        .collect::<Vec<_>>()
        .join(" ")
}

fn quote_arg(arg: &OsStr) -> String {
    let arg = arg.to_string_lossy();

    if !arg.is_empty()
        && arg
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || matches!(ch, '/' | '.' | '-' | '_' | ':' | '='))
    {
        return arg.into_owned();
    }

    format!("'{}'", arg.replace('\'', "'\"'\"'"))
}
