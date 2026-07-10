use argon_test_runner::{JSPlatform, TestSuiteOptions, compiler_test_entrypoint};

compiler_test_entrypoint!(JSPlatform, TestSuiteOptions { optimize_ir: true });
