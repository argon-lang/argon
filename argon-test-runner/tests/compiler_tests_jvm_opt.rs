use argon_test_runner::{compiler_test_entrypoint, JVMPlatform, TestSuiteOptions};

compiler_test_entrypoint!(JVMPlatform, TestSuiteOptions { optimize_ir: true });
