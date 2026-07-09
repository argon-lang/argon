use argon_test_runner::{compiler_test_entrypoint, JSPlatform, TestSuiteOptions};

compiler_test_entrypoint!(JSPlatform, TestSuiteOptions { optimize_ir: true });
