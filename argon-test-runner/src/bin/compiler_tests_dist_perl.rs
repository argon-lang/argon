use argon_test_runner::{
    PerlPlatform, TestSuiteOptions, cmd::CliCommandRunner, suite, workspace::WorkspacePaths,
};
use std::sync::Arc;

fn main() {
    let workspace_paths = WorkspacePaths::from_cargo_manifest_dir().unwrap();
    suite::run(
        Arc::new(PerlPlatform),
        Arc::new(CliCommandRunner::new(workspace_paths)),
        TestSuiteOptions::default(),
    );
}
