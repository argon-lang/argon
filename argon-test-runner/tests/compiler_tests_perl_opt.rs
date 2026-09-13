use argon_test_runner::{PerlPlatform, TestSuiteOptions, compiler_test_entrypoint};

compiler_test_entrypoint!(PerlPlatform, TestSuiteOptions { optimize_ir: true });
