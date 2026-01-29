package dev.argon.testrunner.tests;

import dev.argon.testrunner.*;

public interface NodeCompilerTestSuiteCommon extends CompilerTestSuite {
	@Override
	default HostPlatform hostPlatform() {
		return HostPlatform.Node;
	}

	@Override
	default DriverCommandExecutor createCommandExecutor(RunnerContext context) {
		return new DriverCommandExecutorRPC(context);
	}
	
	
}
