package dev.argon.testrunner.tests;

import dev.argon.testrunner.DriverCommandExecutor;
import dev.argon.testrunner.DriverCommandExecutorCLI;
import dev.argon.testrunner.HostPlatform;
import dev.argon.testrunner.RunnerContext;

public interface JVMCompilerTestSuiteCommon extends CompilerTestSuite {
	@Override
	default HostPlatform hostPlatform() {
		return HostPlatform.JVM;
	}

	@Override
	default DriverCommandExecutor createCommandExecutor(RunnerContext context) {
		return new DriverCommandExecutorCLI(context);
	}


}
