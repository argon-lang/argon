package dev.argon.testrunner;

import dev.argon.vm.api.TubeName;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

public abstract class OutputProgramRunner {
	public abstract String runProgram(Path outputDir, List<LibraryOutputInfo> libraryOutputs) throws IOException, InterruptedException, CommandFailureException;
	
	
	public record LibraryOutputInfo(
		TubeName libraryName,
		Path outputDir
	) {
	}
	
	public static OutputProgramRunner forPlatform(RunnerContext context, String platform) {
		return switch(platform) {
			case "js" -> new JSOutputProgramRunner(context);
			default -> throw new IllegalArgumentException("Unknown platform: " + platform);
		};
	}
}
