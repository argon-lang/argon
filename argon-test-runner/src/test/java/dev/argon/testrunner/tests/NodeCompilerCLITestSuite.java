package dev.argon.testrunner.tests;

import dev.argon.testrunner.DriverCommandExecutor;
import dev.argon.testrunner.DriverCommandExecutorCLI;
import dev.argon.testrunner.GroupedTestCase;
import dev.argon.testrunner.RunnerContext;

import java.util.List;

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
		return testCase.getGroup().equals(List.of("hello_world")) &&
			testCase.getTestCase().getName().equals("Hello World");
	}
}
