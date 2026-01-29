package dev.argon.testrunner.tests;

import dev.argon.testrunner.*;

import java.util.List;

public interface CompilerTestSuite extends AutoCloseable {
	TestResult runTestCase(GroupedTestCase testCase) throws Throwable;

	List<GroupedTestCase> loadTestCases();

	HostPlatform hostPlatform();

	Backend backend();

	OutputProgramRunner createProgramRunner(RunnerContext context);

	DriverCommandExecutor createCommandExecutor(RunnerContext context) throws Exception;
}
