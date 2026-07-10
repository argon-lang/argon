use crate::cmd::{CommandRunner, CommandRunnerPlatform};
use argon_opt::pass::ALL_OPTIMIZATIONS;
use argon_runner::{
    CompileOptions, GenIrOptions, OptimizeOptions,
    local_io::{LocalInputFile, LocalOutputFile, LocalSourceDirectory, StdIoWrite},
};
use argon_testcases::{ExpectedResult, TestCase, load_test_case};
use hashbrown::{HashMap, HashSet};
use libtest_mimic::{Arguments, Conclusion, Trial};
use serde::Deserialize;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tempfile::TempDir;

pub struct TestSuiteContext<P: CompileTargetPlatform, R: CommandRunner + CommandRunnerPlatform<P>> {
    pub platform: Arc<P>,
    pub command_runner: Arc<R>,
    pub test_suite_data_dir: TempDir,
    pub lib_dir: PathBuf,
    pub backend_dir: PathBuf,
    pub print_commands: bool,
    pub options: TestSuiteOptions,

    pub library_platform_metadata: Mutex<HashMap<String, Vec<PathBuf>>>,
    pub library_compiled_tubes: Mutex<HashMap<String, PathBuf>>,
    pub library_generated_ir: Mutex<HashMap<String, PathBuf>>,
    pub platform_state: P::PlatformState,
}

#[derive(Clone, Debug, Default)]
pub struct TestSuiteOptions {
    pub optimize_ir: bool,
}

impl<P: CompileTargetPlatform, R: CommandRunner + CommandRunnerPlatform<P>> TestSuiteContext<P, R> {
    pub fn library_info(self: Arc<Self>, library_name: &str) -> LibraryInfo<P, R> {
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

        let metadata_files = library
            .test_suite_context
            .clone()
            .build_library_metadata(library_name);

        let mut output = Vec::new();
        let success = library.test_suite_context.command_runner.compile(
            CompileOptions {
                tube_name: library.name.parse().unwrap(),
                input_dirs: vec![LocalSourceDirectory::new(library.library_path.join("src"))],
                referenced_tubes: dependency_references
                    .into_iter()
                    .map(LocalInputFile::new)
                    .collect(),
                platform_metadata: metadata_files
                    .into_iter()
                    .map(LocalInputFile::new)
                    .collect(),
                output_file: LocalOutputFile::new(output_file.clone()),
            },
            &mut StdIoWrite::new(&mut output),
        );

        assert!(
            success,
            "Compilation of library {} failed\n{}",
            library.name,
            String::from_utf8_lossy(&output),
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
                let gen_ir_output_file = if library.test_suite_context.options.optimize_ir {
                    library
                        .library_output_path
                        .join(format!("{}.unoptimized.arvm", library.name))
                } else {
                    output_file.clone()
                };

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

                let mut output = Vec::new();
                let success = library.test_suite_context.command_runner.gen_ir(
                    GenIrOptions {
                        input_tube: LocalInputFile::new(input_file),
                        referenced_tubes: dependency_references
                            .into_iter()
                            .map(LocalInputFile::new)
                            .collect(),
                        platform: P::ID.to_owned(),
                        output_file: LocalOutputFile::new(gen_ir_output_file.clone()),
                    },
                    &mut StdIoWrite::new(&mut output),
                );

                assert!(
                    success,
                    "IR Generation of library {} failed\n{}",
                    library.name,
                    String::from_utf8_lossy(&output),
                );

                if library.test_suite_context.options.optimize_ir {
                    let mut output = Vec::new();
                    let success = library.test_suite_context.command_runner.optimize(
                        OptimizeOptions {
                            input_file: LocalInputFile::new(gen_ir_output_file),
                            output_file: LocalOutputFile::new(output_file.clone()),
                            optimizations: default_optimizations(),
                        },
                        &mut StdIoWrite::new(&mut output),
                    );

                    assert!(
                        success,
                        "IR optimization of library {} failed\n{}",
                        library.name,
                        String::from_utf8_lossy(&output),
                    );
                }

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

pub struct TestContext<P: CompileTargetPlatform, R: CommandRunner + CommandRunnerPlatform<P>> {
    pub test_suite_context: Arc<TestSuiteContext<P, R>>,
    pub test_data_dir: PathBuf,
    pub test_case: Arc<(String, TestCase)>,
}

impl<P: CompileTargetPlatform, R: CommandRunner + CommandRunnerPlatform<P>> TestContext<P, R> {
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

        let referenced_tubes = self
            .referenced_libraries()
            .into_iter()
            .map(|library_name| {
                self.test_suite_context
                    .clone()
                    .compile_library_tube(&library_name)
            })
            .map(LocalInputFile::new)
            .collect();

        let mut output = Vec::new();
        let success = self.test_suite_context.command_runner.compile(
            CompileOptions {
                tube_name: "Argon.TestCase".parse().unwrap(),
                input_dirs: vec![LocalSourceDirectory::new(source_dir)],
                referenced_tubes,
                platform_metadata: Vec::new(),
                output_file: LocalOutputFile::new(output_file.clone()),
            },
            &mut StdIoWrite::new(&mut output),
        );

        if success {
            Ok(output_file)
        } else {
            Err(String::from_utf8_lossy(&output).into_owned())
        }
    }

    pub fn genir_test_case(&self) -> PathBuf {
        let output_file = self.test_data_dir.join("Argon.TestCase.arvm");
        let gen_ir_output_file = if self.test_suite_context.options.optimize_ir {
            self.test_data_dir.join("Argon.TestCase.unoptimized.arvm")
        } else {
            output_file.clone()
        };
        let input_file = self.compile_test_case().unwrap();

        let referenced_tubes = self
            .referenced_libraries()
            .into_iter()
            .map(|library_name| {
                self.test_suite_context
                    .clone()
                    .compile_library_tube(&library_name)
            })
            .map(LocalInputFile::new)
            .collect();

        let mut output = Vec::new();
        let success = self.test_suite_context.command_runner.gen_ir(
            GenIrOptions {
                input_tube: LocalInputFile::new(input_file),
                referenced_tubes,
                platform: P::ID.to_owned(),
                output_file: LocalOutputFile::new(gen_ir_output_file.clone()),
            },
            &mut StdIoWrite::new(&mut output),
        );

        assert!(
            success,
            "IR generation of test case failed\n{}",
            String::from_utf8_lossy(&output),
        );

        if self.test_suite_context.options.optimize_ir {
            let mut output = Vec::new();
            let success = self.test_suite_context.command_runner.optimize(
                OptimizeOptions {
                    input_file: LocalInputFile::new(gen_ir_output_file),
                    output_file: LocalOutputFile::new(output_file.clone()),
                    optimizations: default_optimizations(),
                },
                &mut StdIoWrite::new(&mut output),
            );

            assert!(
                success,
                "IR optimization of test case failed\n{}",
                String::from_utf8_lossy(&output),
            );
        }

        output_file
    }
}

fn default_optimizations() -> Vec<String> {
    ALL_OPTIMIZATIONS
        .iter()
        .map(|optimization| optimization.name().to_owned())
        .collect()
}

pub struct LibraryInfo<P: CompileTargetPlatform, R: CommandRunner + CommandRunnerPlatform<P>> {
    pub name: String,
    pub test_suite_context: Arc<TestSuiteContext<P, R>>,
    pub library_path: PathBuf,
    pub library_output_path: PathBuf,
}

pub trait CompileTargetPlatform: Sized + Send + Sync + 'static {
    type PlatformState: Default + Send + Sync + 'static;
    type PlatformMetadataOptions<I, O>;
    type CodeGenOptions<I, O>;

    const ID: &'static str;

    fn library_platform_metadata<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        library: &LibraryInfo<Self, R>,
    ) -> Vec<PathBuf>;
    fn codegen<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        test_context: &TestContext<Self, R>,
    );
    fn run<R: CommandRunner + CommandRunnerPlatform<Self>>(
        self: Arc<Self>,
        test_context: &TestContext<Self, R>,
    ) -> TestExecutionResult;
}

pub struct TestExecutionResult {
    pub exit_code: i32,
    pub output: String,
}

fn result_matches_expected(result: &TestExecutionResult, expected: &ExpectedResult) -> bool {
    match expected {
        ExpectedResult::Output(expected_output) => {
            fn normalize_output(output: &str) -> String {
                output
                    .split('\n')
                    .map(|line| line.trim())
                    .filter(|line| !line.is_empty())
                    .collect::<String>()
            }

            result.exit_code == 0
                && normalize_output(&result.output) == normalize_output(expected_output)
        }
        ExpectedResult::CompileErrors(_) => false,
        ExpectedResult::ExecutionErrors(expected_errors) => {
            result.exit_code != 0
                && expected_errors
                    .iter()
                    .all(|expected_error| result.output.contains(expected_error))
        }
    }
}

fn run_test<P: CompileTargetPlatform, R: CommandRunner + CommandRunnerPlatform<P>>(
    context: TestContext<P, R>,
) {
    match (context.compile_test_case(), &context.test_case.1.expected) {
        (Ok(_), ExpectedResult::CompileErrors(expected_errors)) => {
            panic!(
                "Expected compilation to fail with errors: {:?}",
                expected_errors
            );
        }
        (Ok(_), _) => {}

        (Err(output), ExpectedResult::CompileErrors(expected_errors)) => {
            assert!(
                expected_errors
                    .iter()
                    .all(|expected_error| output.contains(expected_error)),
                "Expected compilation to fail with errors: {:?}\nActual:\n{}",
                expected_errors,
                output,
            );
            return;
        }

        (Err(output), _) => {
            panic!(
                "Expected compilation to succeed, but it failed with:\n{}",
                output
            );
        }
    };
    let platform = context.test_suite_context.platform.clone();
    platform.clone().codegen(&context);
    let result = platform.run(&context);

    assert!(
        result_matches_expected(&result, &context.test_case.1.expected),
        "Output mismatch\nActual:{}\nExpected:{:?}",
        result.output,
        context.test_case.1.expected,
    );
}

pub fn build_test_suite<P, R>(
    platform: Arc<P>,
    command_runner: Arc<R>,
    options: TestSuiteOptions,
    test_cases: &[Arc<(String, TestCase)>],
    tests: &mut Vec<Trial>,
) where
    P: CompileTargetPlatform,
    R: CommandRunner + CommandRunnerPlatform<P> + Send + Sync + 'static,
{
    let temp_dir = TempDir::new().unwrap();
    let workspace_paths = crate::workspace::WorkspacePaths::from_cargo_manifest_dir().unwrap();

    let context = Arc::new(TestSuiteContext {
        platform,
        command_runner,
        test_suite_data_dir: temp_dir,
        lib_dir: workspace_paths.libraries_dir(),
        backend_dir: workspace_paths.backend_dir(),
        print_commands: false,
        options,

        library_platform_metadata: Mutex::new(HashMap::new()),
        library_compiled_tubes: Mutex::new(HashMap::new()),
        library_generated_ir: Mutex::new(HashMap::new()),
        platform_state: Default::default(),
    });

    for test_case_pair in test_cases {
        let (test_name, _) = &**test_case_pair;
        let context = context.clone();
        let test_case_pair = test_case_pair.clone();

        tests.push(Trial::test(test_name.clone(), move || {
            let (test_name, _) = &*test_case_pair;
            let test_data_dir = context.test_suite_data_dir.path().join(test_name);

            let test_context = TestContext {
                test_suite_context: context,
                test_data_dir,
                test_case: test_case_pair,
            };

            run_test(test_context);
            Ok(())
        }));
    }
}

fn load_test_cases() -> Vec<Arc<(String, TestCase)>> {
    let workspace_paths = crate::workspace::WorkspacePaths::from_cargo_manifest_dir().unwrap();
    let testcases_dir = workspace_paths.testcases_dir();

    let mut test_cases = Vec::new();
    for entry in walkdir::WalkDir::new(&testcases_dir) {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
            let test_case = load_test_case(path).unwrap();
            let rel_path = path
                .strip_prefix(&testcases_dir)
                .unwrap()
                .to_string_lossy()
                .into_owned();

            test_cases.push(Arc::new((
                rel_path.strip_suffix(".xml").unwrap().to_string(),
                test_case,
            )));
        }
    }

    test_cases
}

pub fn run<P, R>(platform: Arc<P>, command_runner: Arc<R>, options: TestSuiteOptions)
where
    P: CompileTargetPlatform,
    R: CommandRunner + CommandRunnerPlatform<P> + Send + Sync + 'static,
{
    run_conclusion(platform, command_runner, options).exit()
}

pub fn run_conclusion<P, R>(
    platform: Arc<P>,
    command_runner: Arc<R>,
    options: TestSuiteOptions,
) -> Conclusion
where
    P: CompileTargetPlatform,
    R: CommandRunner + CommandRunnerPlatform<P> + Send + Sync + 'static,
{
    let args = Arguments::from_args();
    let test_cases = load_test_cases();
    let mut tests = Vec::new();

    build_test_suite(platform, command_runner, options, &test_cases, &mut tests);

    libtest_mimic::run(&args, tests)
}
