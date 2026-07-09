use argon_test_runner::{
    cmd::CliCommandRunner, suite, workspace::WorkspacePaths, JSPlatform, TestSuiteOptions,
};
use std::sync::Arc;

fn main() {
    let workspace_paths = WorkspacePaths::from_cargo_manifest_dir().unwrap();
    suite::run(
        Arc::new(JSPlatform),
        Arc::new(CliCommandRunner::new(workspace_paths)),
        TestSuiteOptions::default(),
    );
}
