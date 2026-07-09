use argon_test_runner::cmd::{CommandRunnerPlatform, DirectCommandRunner};
use argon_test_runner::{
    workspace::WorkspacePaths, CompileTargetPlatform, JSPlatform, JVMPlatform, TestContext,
    TestSuiteContext, TestSuiteOptions,
};
use argon_testcases::load_test_case;
use clap::{Parser, ValueEnum};
use hashbrown::HashMap;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tempfile::TempDir;

#[derive(Parser)]
#[command(about = "Compile Argon test XML files without running them")]
struct Args {
    #[arg(long)]
    platform: Platform,

    #[arg(long, value_name = "DIR", default_value = ".")]
    output_dir: PathBuf,

    #[arg(long)]
    optimize_ir: bool,

    #[arg(value_name = "TEST_XML", required = true)]
    test_xml_files: Vec<PathBuf>,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Platform {
    Js,
    Jvm,
}

impl std::fmt::Display for Platform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Js => f.write_str("js"),
            Self::Jvm => f.write_str("jvm"),
        }
    }
}

fn create_temp_dir(output_dir: &Path) -> Result<TempDir, Box<dyn Error>> {
    let mut builder = tempfile::Builder::new();
    builder.prefix("argon-testcases-compile-");
    builder.disable_cleanup(true);

    std::fs::create_dir_all(output_dir)?;
    let temp_dir = builder.tempdir_in(output_dir)?;

    Ok(temp_dir)
}

fn create_suite_context<P>(
    platform: Arc<P>,
    command_runner: Arc<DirectCommandRunner>,
    temp_dir: TempDir,
    workspace_paths: &WorkspacePaths,
    options: TestSuiteOptions,
) -> Arc<TestSuiteContext<P, DirectCommandRunner>>
where
    P: CompileTargetPlatform,
    DirectCommandRunner: CommandRunnerPlatform<P>,
{
    Arc::new(TestSuiteContext {
        platform,
        command_runner,
        test_suite_data_dir: temp_dir,
        lib_dir: workspace_paths.libraries_dir(),
        backend_dir: workspace_paths.backend_dir(),
        print_commands: true,
        options,

        library_platform_metadata: Mutex::new(HashMap::new()),
        library_compiled_tubes: Mutex::new(HashMap::new()),
        library_generated_ir: Mutex::new(HashMap::new()),
        platform_state: Default::default(),
    })
}

fn test_output_name(index: usize, test_xml_file: &Path) -> String {
    let file_stem = test_xml_file
        .file_stem()
        .and_then(|file_stem| file_stem.to_str())
        .unwrap_or("testcase");

    let name = file_stem
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                ch
            } else {
                '_'
            }
        })
        .collect::<String>();

    format!("{index:04}-{name}")
}

fn compile_testcases<P>(
    context: Arc<TestSuiteContext<P, DirectCommandRunner>>,
    test_xml_files: &[PathBuf],
) -> Result<bool, Box<dyn Error>>
where
    P: CompileTargetPlatform,
    DirectCommandRunner: CommandRunnerPlatform<P>,
{
    let mut compiled_all = true;

    for (index, test_xml_file) in test_xml_files.iter().enumerate() {
        let test_case = load_test_case(test_xml_file)?;
        let test_output_name = test_output_name(index, test_xml_file);
        let test_data_dir = context.test_suite_data_dir.path().join(&test_output_name);
        let test_name = test_case.name.clone();
        let test_case = Arc::new((test_output_name, test_case));

        let test_context = TestContext {
            test_suite_context: context.clone(),
            test_data_dir,
            test_case,
        };

        match test_context.compile_test_case() {
            Ok(output_file) => {
                let platform = test_context.test_suite_context.platform.clone();
                platform.codegen(&test_context);

                println!(
                    "compiled {} ({}) -> {}",
                    test_xml_file.display(),
                    test_name,
                    output_file.display()
                );
                println!(
                    "generated outputs -> {}",
                    test_context.test_data_dir.display()
                );
            }
            Err(output) => {
                compiled_all = false;
                eprintln!(
                    "failed to compile {} ({})\n{}",
                    test_xml_file.display(),
                    test_name,
                    output
                );
            }
        }
    }

    Ok(compiled_all)
}

fn run() -> Result<i32, Box<dyn Error>> {
    let args = Args::parse();
    let workspace_paths = WorkspacePaths::from_cargo_manifest_dir()?;
    let temp_dir = create_temp_dir(&args.output_dir)?;
    let output_root = temp_dir.path().to_owned();
    let command_runner = Arc::new(DirectCommandRunner::new(workspace_paths.clone()));

    let compiled_all = match args.platform {
        Platform::Js => {
            let context = create_suite_context(
                Arc::new(JSPlatform),
                command_runner.clone(),
                temp_dir,
                &workspace_paths,
                TestSuiteOptions {
                    optimize_ir: args.optimize_ir,
                },
            );
            compile_testcases(context, &args.test_xml_files)?
        }
        Platform::Jvm => {
            let context = create_suite_context(
                Arc::new(JVMPlatform),
                command_runner,
                temp_dir,
                &workspace_paths,
                TestSuiteOptions {
                    optimize_ir: args.optimize_ir,
                },
            );
            compile_testcases(context, &args.test_xml_files)?
        }
    };

    println!("output root: {}", output_root.display());

    Ok(if compiled_all { 0 } else { 1 })
}

fn main() {
    match run() {
        Ok(code) => std::process::exit(code),
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}
