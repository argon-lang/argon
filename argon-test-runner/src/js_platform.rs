use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use std::sync::{Arc, Mutex};
use crate::{CompileTargetPlatform, LibraryInfo, TestContext};

pub struct JSPlatform;

#[derive(Default)]
pub struct JSPlatformState {
    library_codegen: Mutex<HashMap<String, PathBuf>>,
}

impl CompileTargetPlatform for JSPlatform {
    type PlatformState = JSPlatformState;

    const ID: &'static str = "js";


    fn library_platform_metadata(self: Arc<Self>, library: &LibraryInfo<Self::PlatformState>) -> Vec<PathBuf> {
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

        vec![ path ]
    }

    fn codegen(self: Arc<Self>, test_context: &TestContext<Self::PlatformState>) {
        todo!()
    }

    fn run(self: Arc<Self>, test_context: &TestContext<Self::PlatformState>) {
        todo!()
    }
}

