use argon_test_runner::{JVMPlatform, cmd::DirectCommandRunner, suite, workspace::WorkspacePaths};
use std::sync::Arc;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

#[cfg(feature = "dhat-heap")]
fn main() -> std::process::ExitCode {
    let profiler = dhat::Profiler::new_heap();
    let conclusion = run_tests();
    drop(profiler);

    conclusion.exit_code()
}

#[cfg(not(feature = "dhat-heap"))]
fn main() {
    run_tests().exit()
}

fn run_tests() -> libtest_mimic::Conclusion {
    let workspace_paths = WorkspacePaths::from_cargo_manifest_dir().unwrap();
    suite::run_conclusion(
        Arc::new(JVMPlatform),
        Arc::new(DirectCommandRunner::new(workspace_paths)),
    )
}
