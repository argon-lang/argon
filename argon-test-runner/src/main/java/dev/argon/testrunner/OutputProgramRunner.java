package dev.argon.testrunner;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

abstract class OutputProgramRunner {
	public abstract String runProgram(Path outputDir, List<LibraryOutputInfo> libraryOutputs) throws IOException, InterruptedException, CommandFailureException;
	
	
	public record LibraryOutputInfo(
		String libraryName,
		Path outputDir
	) {
	}
	
	public static OutputProgramRunner forPlatform(String platform) {
		return switch(platform) {
			case "js" -> new JSOutputProgramRunner();
			default -> throw new IllegalArgumentException("Unknown platform: " + platform);
		};
	}
}
