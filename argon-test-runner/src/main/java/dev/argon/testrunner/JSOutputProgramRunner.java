package dev.argon.testrunner;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.file.PathUtils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

class JSOutputProgramRunner extends OutputProgramRunner {
	@Override
	public String runProgram(Path outputDir, List<LibraryOutputInfo> libraryOutputs) throws IOException, InterruptedException, CommandFailureException {
		for(var libraryOutput : libraryOutputs) {
			var libPath = outputDir.resolve("node_modules/@argon-tube", libraryOutput.libraryName());
			Files.createDirectories(libPath.getParent());
			PathUtils.copyDirectory(libraryOutput.outputDir(), libPath);
		}

		Files.writeString(outputDir.resolve("main.js"), "import { main$a$t$e$r$t$e } from \"./index.js\"; main$a$t$e$r$t$e();");
		
		var pb = new ProcessBuilder("node", outputDir.resolve("main.js").toString());
		pb.redirectErrorStream(true);
		pb.redirectOutput(ProcessBuilder.Redirect.PIPE);
		var process = pb.start();
		
		String output;
		try(var is = process.getInputStream()) {
			output = IOUtils.toString(is, StandardCharsets.UTF_8);
		}
		
		int exitCode = process.waitFor();
		if(exitCode != 0) {
			throw new CommandFailureException("Command completed with exit code " + exitCode + ": " + new ArrayList<>(pb.command()), output);
		}
		
		return output;
	}
}
