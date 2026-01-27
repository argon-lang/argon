package dev.argon.testrunner;

public record CommandExecutionResult(
	int exitCode,
	String output
) {}
