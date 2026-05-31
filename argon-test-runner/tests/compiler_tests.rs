use argon_test_runner::*;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use std::sync::{Arc, Mutex};
use libtest_mimic::{Arguments, Trial};
use tempfile::TempDir;
use argon_testcases::{load_test_case, TestCase};

fn workspace_root() -> PathBuf {
    let mut workspace_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    workspace_dir.pop();
    workspace_dir
}

fn library_info<P>(context: Arc<TestSuiteContext<P>>, library_name: &str) -> LibraryInfo<P> {
    let library_path = context.lib_dir.join(library_name);
    let library_output_path = context.test_suite_data_dir.path().join("lib").join(library_name);

    std::fs::create_dir_all(&library_output_path).unwrap();

    LibraryInfo {
        name: library_name.to_owned(),
        test_suite_context: context,
        library_path,
        library_output_path,
    }
}

fn build_library_metadata<P: CompileTargetPlatform>(platform: Arc<P>, context: Arc<TestSuiteContext<P::PlatformState>>, library_name: &str) -> Vec<PathBuf> {
    let mut metadata_map = context.library_platform_metadata.lock().unwrap();

    let context2 = context.clone();

    metadata_map.entry(library_name.to_owned()).or_insert_with(|| {
        platform.library_platform_metadata(&library_info(context2, library_name))
    }).clone()
}

fn build_library_tube<P: CompileTargetPlatform>(platform: Arc<P>, context: Arc<TestSuiteContext<P::PlatformState>>, library_name: &str) -> PathBuf {
    let mut tube_map = context.library_compiled_tubes.lock().unwrap();

    let context2 = context.clone();

    tube_map.entry(library_name.to_owned()).or_insert_with(|| {
        let library = library_info(context2, library_name);

        let output_file = library.library_output_path.join(format!("{}.artube", library.name));

        let mut cmd = Command::new(&library.test_suite_context.argon_bin);
        cmd.arg("compile");
        cmd.arg("--tube-name");
        cmd.arg(&library.name);

        cmd.arg("--input");
        cmd.arg(library.library_path.join("src"));

        cmd.arg("--output-file");
        cmd.arg(&output_file);

        let metadata_files = build_library_metadata(platform, library.test_suite_context.clone(), library_name);

        for metadata_file in metadata_files {
            cmd.arg("--platform");
            cmd.arg(metadata_file);
        }

        let output = cmd.output().unwrap();

        assert!(output.status.success(), "Compilation of library {} failed", library.name);

        output_file
    }).clone()
}

fn compile_test_case<P: CompileTargetPlatform>(platform: Arc<P>, context: &TestContext<P::PlatformState>, test_case: &TestCase) -> PathBuf {
    let source_dir = context.test_data_dir.join("src");

    std::fs::create_dir_all(&source_dir).unwrap();

    for f in &test_case.input_sources {
        let source_path = source_dir.join(&f.name);
        std::fs::write(source_path, &f.source).unwrap();
    }

    let output_file = context.test_data_dir.join("Argon.TestCase.artube");

    let mut cmd = Command::new(&context.test_suite_context.argon_bin);
    cmd.arg("compile");
    cmd.arg("--tube-name");
    cmd.arg("Argon.TestCase");
    cmd.arg("--input");
    cmd.arg(source_dir);
    cmd.arg("--output-file");
    cmd.arg(&output_file);

    let mut library_names = Vec::new();
    library_names.push("Argon.Core".to_owned());
    library_names.extend(test_case.libraries.iter().cloned());

    for library_name in library_names {
        let tube_path = build_library_tube(platform.clone(), context.test_suite_context.clone(), &library_name);
        cmd.arg("--reference");
        cmd.arg(tube_path);
    }

    let output = cmd.output().unwrap();

    assert!(output.status.success(), "Compilation of test case failed");

    output_file
}

fn run_test<P: CompileTargetPlatform>(platform: Arc<P>, context: TestContext<P::PlatformState>, test_case: &TestCase) {
    compile_test_case(platform.clone(), &context, test_case);
    platform.clone().codegen(&context);
    platform.run(&context);
}

pub fn build_test_suite<P: CompileTargetPlatform>(platform: Arc<P>, test_cases: &Vec<Arc<(String, TestCase)>>, tests: &mut Vec<Trial>) {
    let temp_dir = TempDir::new().unwrap();

    let mut lib_dir = workspace_root();
    lib_dir.push("libraries");

    let mut argon_bin = workspace_root();
    argon_bin.push("dist/bin/argonc");

    let context = Arc::new(TestSuiteContext {
        test_suite_data_dir: temp_dir,
        lib_dir,
        argon_bin,

        library_platform_metadata: Mutex::new(HashMap::new()),
        library_compiled_tubes: Mutex::new(HashMap::new()),
        platform_state: Default::default(),
    });


    for test_case_pair in test_cases {
        let (test_name, _) = &**test_case_pair;
        let platform = platform.clone();
        let context = context.clone();
        let test_case_pair = test_case_pair.clone();

        tests.push(Trial::test(test_name.clone(), move || {
            let (test_name, test_case) = &*test_case_pair;
            let test_data_dir = context.test_suite_data_dir.path().join(test_name);

            let test_context = TestContext {
                test_suite_context: context,
                test_data_dir,
            };

            run_test(platform, test_context, test_case);
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
            let rel_path = path.strip_prefix(&testcases_dir).unwrap().to_string_lossy().into_owned();

            test_cases.push(Arc::new((rel_path, test_case)));
        }
    }

    test_cases.retain(|pair| pair.0.ends_with("HelloWorld.xml"));

    let mut tests = Vec::new();

    build_test_suite(Arc::new(JSPlatform), &test_cases, &mut tests);

    libtest_mimic::run(&args, tests).exit()
}

