package dev.argon.testrunner.tests;

import dev.argon.testrunner.*;

public class JVMCompilerRPCTestSuite
	extends CompilerTestSuiteBase
	implements JVMCompilerTestSuiteCommon,
	           JSBackendTestSuiteCommon
{
	@Override
	public DriverCommandExecutor createCommandExecutor(RunnerContext context) {
		return new DriverCommandExecutorRPC(context);
	}

	@Override
	protected boolean useTestCase(GroupedTestCase testCase) {
		return !testCase.getGroup().isEmpty() && testCase.getGroup().getFirst().equals("hello_world");
	}
}
