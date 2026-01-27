package dev.argon.testrunner.tests;

import dev.argon.testrunner.Backend;
import dev.argon.testrunner.JSOutputProgramRunner;
import dev.argon.testrunner.OutputProgramRunner;
import dev.argon.testrunner.RunnerContext;

public interface JSBackendTestSuiteCommon extends CompilerTestSuite {
	@Override
	default Backend backend() {
		return Backend.JS;
	}

	@Override
	default OutputProgramRunner createProgramRunner(RunnerContext context) {
		return new JSOutputProgramRunner(context);
	}
}
