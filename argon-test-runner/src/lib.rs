mod js_platform;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tempfile::TempDir;

pub use js_platform::*;

pub struct TestSuiteContext<S> {
    pub test_suite_data_dir: TempDir,
    pub lib_dir: PathBuf,
    pub argon_bin: PathBuf,

    pub library_platform_metadata: Mutex<HashMap<String, Vec<PathBuf>>>,
    pub library_compiled_tubes: Mutex<HashMap<String, PathBuf>>,
    pub platform_state: S,
}

pub struct TestContext<S> {
    pub test_suite_context: Arc<TestSuiteContext<S>>,
    pub test_data_dir: PathBuf,
}

pub struct LibraryInfo<S> {
    pub name: String,
    pub test_suite_context: Arc<TestSuiteContext<S>>,
    pub library_path: PathBuf,
    pub library_output_path: PathBuf,
}

pub trait CompileTargetPlatform: Send + Sync + 'static {
    type PlatformState: Default + Send + Sync + 'static;

    const ID: &'static str;

    fn library_platform_metadata(self: Arc<Self>, library: &LibraryInfo<Self::PlatformState>) -> Vec<PathBuf>;
    fn codegen(self: Arc<Self>, test_context: &TestContext<Self::PlatformState>);
    fn run(self: Arc<Self>, test_context: &TestContext<Self::PlatformState>);
}

pub struct TestExecutionResult {
    pub exit_code: i32,
    pub output: String,
}

