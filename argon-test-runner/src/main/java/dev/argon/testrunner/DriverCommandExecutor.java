package dev.argon.testrunner;

import dev.argon.driver.api.command.DriverCommand;

public interface DriverCommandExecutor extends AutoCloseable {
	CommandExecutionResult execute(
		DriverCommand<String, String, String, String> command
	) throws Throwable;
}
