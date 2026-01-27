package dev.argon.testrunner;

public interface TestResult {
	record Executed(String output) implements TestResult {}
	record CompileError(String output) implements TestResult {}
}
