use crate::{CompileTargetPlatform, LibraryInfo, TestContext, TestExecutionResult};
use fs_extra::dir::CopyOptions;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use std::sync::{Arc, Mutex};

pub struct JSPlatform;

#[derive(Default)]
pub struct JSPlatformState {
    library_codegen: Mutex<HashMap<String, PathBuf>>,
}

impl JSPlatform {
    fn codegen_library(&self, library_info: &LibraryInfo<Self>) -> PathBuf {
        let mut tube_map = library_info
            .test_suite_context
            .platform_state
            .library_codegen
            .lock()
            .unwrap();

        let context = library_info.test_suite_context.clone();

        tube_map
            .entry(library_info.name.clone())
            .or_insert_with(|| {
                let library = context.library_info(&library_info.name);

                let input_file = library
                    .test_suite_context
                    .clone()
                    .genir_library_tube(&library_info.name);
                let output_dir = library.library_output_path.join("js");
                std::fs::create_dir_all(&output_dir).unwrap();

                let mut cmd = Command::new(&library.test_suite_context.argon_bin);
                cmd.arg("codegen");
                cmd.arg("js");

                cmd.arg("--input");
                cmd.arg(input_file);

                cmd.arg("--output");
                cmd.arg(&output_dir);

                let output = cmd.output().unwrap();

                assert!(
                    output.status.success(),
                    "Code generation of library {} failed\n{}\n{}",
                    library.name,
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                );

                output_dir
            })
            .clone()
    }
}

impl CompileTargetPlatform for JSPlatform {
    type PlatformState = JSPlatformState;

    const ID: &'static str = "js";

    fn library_platform_metadata(self: Arc<Self>, library: &LibraryInfo<Self>) -> Vec<PathBuf> {
        let path = library.library_output_path.join("platform-metadata.esx");

        let mut cmd = Command::new(&library.test_suite_context.argon_bin);
        cmd.arg("platform-metadata");
        cmd.arg("js");

        let js_dir = library.library_path.join("js");
        if js_dir.exists() {
            for entry in std::fs::read_dir(js_dir).unwrap() {
                let entry = entry.unwrap();
                let path = entry.path();
                if path.is_file() {
                    cmd.arg("--extern");
                    cmd.arg(path);
                }
            }
        }

        cmd.arg("--output-file");
        cmd.arg(&path);

        let output = cmd.output().unwrap();

        assert!(
            output.status.success(),
            "Getting metadata for library {} failed:\n{}\n{}",
            library.name,
            String::from_utf8_lossy(&*output.stdout),
            String::from_utf8_lossy(&*output.stderr),
        );

        vec![path]
    }

    fn codegen(self: Arc<Self>, test_context: &TestContext<Self>) {
        let output_dir = test_context.test_data_dir.join("js");
        std::fs::create_dir_all(&output_dir).unwrap();

        {
            let runtime_dir = test_context
                .test_suite_context
                .backend_dir
                .join("runtime/js");
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
                    .library_info(library_name),
            );

            let lib_module_dir =
                output_dir.join(format!("node_modules/@argon-tube/{}", library_name));
            std::fs::create_dir_all(&lib_module_dir).unwrap();

            let mut options = CopyOptions::new();
            options.content_only = true;
            fs_extra::dir::copy(&lib_dir, &lib_module_dir, &options).unwrap();
        }

        let input_file = test_context.genir_test_case();

        let mut cmd = Command::new(&test_context.test_suite_context.argon_bin);
        cmd.arg("codegen");
        cmd.arg("js");

        cmd.arg("--input");
        cmd.arg(input_file);
        cmd.arg("--output");
        cmd.arg(&output_dir);

        let output = cmd.output().unwrap();

        assert!(
            output.status.success(),
            "Code generation of test case failed\n{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );

        {
            let entrypoint_path = output_dir.join("main.js");
            let contents = "import * as rt from \"@argon-lang/runtime\";\n\
				import { main$a$t$e$r$t$e } from \"./index.js\";\n\
				rt.resolve(main$a$t$e$r$t$e());\n";

            std::fs::write(&entrypoint_path, contents).unwrap();
        }
    }

    fn run(self: Arc<Self>, test_context: &TestContext<Self>) -> TestExecutionResult {
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
