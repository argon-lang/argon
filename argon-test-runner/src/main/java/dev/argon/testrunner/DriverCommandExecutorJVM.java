package dev.argon.testrunner;

import dev.argon.backend.api.BackendFactory;
import dev.argon.driver.api.CompilerDriver;
import dev.argon.driver.api.CompilerDriverOptions;
import dev.argon.driver.api.command.DriverCommand;
import dev.argon.driver.launcher.ArgonLauncher;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

public class DriverCommandExecutorJVM implements DriverCommandExecutor {
	public DriverCommandExecutorJVM(RunnerContext context) throws IOException {
		this.backendFactories = ArgonLauncher.loadBackendFactories(driver, context.distDir().resolve("backends"));
	}
	
	private final CompilerDriver driver = new dev.argon.driver.CompilerDriver();
	private final List<BackendFactory> backendFactories;
	
	
	@Override
	public CommandExecutionResult execute(DriverCommand<String, String, String, String> command) throws Throwable {
		var bytes = new ByteArrayOutputStream();
		var outputStream = new PrintStream(bytes, true, StandardCharsets.UTF_8);

		var options = new CompilerDriverOptions(
			backendFactories,
			ArgonLauncher.realizeCommandPath(command),
			InputStream.nullInputStream(),
			outputStream,
			outputStream
		);
		
		int exitCode = driver.runCommand(options);
		
		return new CommandExecutionResult(exitCode, bytes.toString(StandardCharsets.UTF_8));
	}

	@Override
	public void close() throws Exception {}
}
