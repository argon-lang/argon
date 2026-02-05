package dev.argon.testrunner.tests;

import dev.argon.testrunner.*;

public interface JVMCompilerTestSuiteCommon extends CompilerTestSuite {
	@Override
	default HostPlatform hostPlatform() {
		return HostPlatform.JVM;
	}

	@Override
	default DriverCommandExecutor createCommandExecutor(RunnerContext context) throws Exception {
		return new DriverCommandExecutorRPC(context);
	}


}
