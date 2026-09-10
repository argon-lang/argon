pub mod cmd;
mod js_platform;
mod jvm_platform;
pub mod suite;
pub mod workspace;

pub use js_platform::*;
pub use jvm_platform::*;
pub use suite::{
    CompileTargetPlatform, LibraryInfo, TestContext, TestExecutionResult, TestSuiteContext,
    TestSuiteOptions,
};

#[macro_export]
macro_rules! compiler_test_entrypoint {
    ($platform:expr, $options:expr $(,)?) => {
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
            if std::env::var_os("RUST_MIN_STACK").is_none() {
                // SAFETY: This is the test executable's entrypoint, before the test runner has
                // spawned any worker threads that could access the process environment.
                unsafe { std::env::set_var("RUST_MIN_STACK", "8388608") };
            }
            let workspace_paths =
                $crate::workspace::WorkspacePaths::from_cargo_manifest_dir().unwrap();
            $crate::suite::run_conclusion(
                std::sync::Arc::new($platform),
                std::sync::Arc::new($crate::cmd::DirectCommandRunner::new(workspace_paths)),
                $options,
            )
        }
    };
}
