package dev.argon.testrunner;

import java.util.Arrays;
import java.util.stream.Collectors;

public interface ExpectedTestResult {
	boolean matchedBy(TestResult actual);

	record Executed(String output) implements ExpectedTestResult {
		@Override
		public boolean matchedBy(TestResult actual) {
			return actual instanceof TestResult.Executed(var actualOutput) &&
				normalize(actualOutput).equals(normalize(output));
		}

		private String normalize(String s) {
			return Arrays.stream(s.trim().split("\\n"))
				.map(String::trim)
				.collect(Collectors.joining());
		}
	}
	record CompileError(String output) implements ExpectedTestResult {
		@Override
		public boolean matchedBy(TestResult actual) {
			return actual instanceof TestResult.CompileError(var actualOutput) &&
				actualOutput.contains(output);
		}
	}
}
