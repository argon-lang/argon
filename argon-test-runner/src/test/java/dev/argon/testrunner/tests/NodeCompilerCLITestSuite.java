package dev.argon.testrunner.tests;

import dev.argon.testrunner.DriverCommandExecutor;
import dev.argon.testrunner.DriverCommandExecutorCLI;
import dev.argon.testrunner.GroupedTestCase;
import dev.argon.testrunner.RunnerContext;

public class NodeCompilerCLITestSuite
	extends CompilerTestSuiteBase
	implements NodeCompilerTestSuiteCommon,
	           JSBackendTestSuiteCommon
{
	@Override
	public DriverCommandExecutor createCommandExecutor(RunnerContext context) {
		return new DriverCommandExecutorCLI(context);
	}

	@Override
	protected boolean useTestCase(GroupedTestCase testCase) {
		return !testCase.getGroup().isEmpty() && testCase.getGroup().getFirst().equals("hello_world");
	}
}
