use argon_test_runner::{JVMPlatform, TestSuiteOptions, compiler_test_entrypoint};

compiler_test_entrypoint!(JVMPlatform, TestSuiteOptions { optimize_ir: true });
