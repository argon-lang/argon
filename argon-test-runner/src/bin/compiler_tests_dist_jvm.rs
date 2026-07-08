use argon_test_runner::{JVMPlatform, cmd::CliCommandRunner, suite, workspace::WorkspacePaths};
use std::sync::Arc;

fn main() {
    let workspace_paths = WorkspacePaths::from_cargo_manifest_dir().unwrap();
    suite::run(
        Arc::new(JVMPlatform),
        Arc::new(CliCommandRunner::new(workspace_paths)),
    );
}
