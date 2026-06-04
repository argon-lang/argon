use argon_test_runner::*;
use argon_testcases::{ExpectedResult, TestCase, load_test_case};
use libtest_mimic::{Arguments, Trial};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tempfile::TempDir;

fn workspace_root() -> PathBuf {
    let mut workspace_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    workspace_dir.pop();
    workspace_dir
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
                && normalize_output(&result.output) == normalize_output(&expected_output)
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

fn run_test<P: CompileTargetPlatform>(context: TestContext<P>) {
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

pub fn build_test_suite<P: CompileTargetPlatform>(
    platform: Arc<P>,
    test_cases: &Vec<Arc<(String, TestCase)>>,
    tests: &mut Vec<Trial>,
) {
    let temp_dir = TempDir::new().unwrap();

    let mut lib_dir = workspace_root();
    lib_dir.push("libraries");

    let mut argon_bin = workspace_root();
    argon_bin.push("dist/bin/argonc");

    let mut backend_dir = workspace_root();
    backend_dir.push("backend");

    let context = Arc::new(TestSuiteContext {
        platform,
        test_suite_data_dir: temp_dir,
        lib_dir,
        argon_bin,
        backend_dir,

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

fn main() {
    let args = Arguments::from_args();

    let mut testcases_dir = workspace_root();
    testcases_dir.push("argon-testcases/testcases");

    let mut test_cases = Vec::new();
    for entry in walkdir::WalkDir::new(&testcases_dir) {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
            let test_case = load_test_case(&path).unwrap();
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

    let mut tests = Vec::new();

    build_test_suite(Arc::new(JSPlatform), &test_cases, &mut tests);

    libtest_mimic::run(&args, tests).exit()
}
