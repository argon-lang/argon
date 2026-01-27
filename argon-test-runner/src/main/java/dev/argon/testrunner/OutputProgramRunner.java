package dev.argon.testrunner;

import dev.argon.vm.api.TubeName;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

public interface OutputProgramRunner {
	String runProgram(Path outputDir, List<LibraryOutputInfo> libraryOutputs) throws IOException, InterruptedException, CommandFailureException;
	
	
	record LibraryOutputInfo(
		TubeName libraryName,
		Path outputDir
	) {
	}
}
