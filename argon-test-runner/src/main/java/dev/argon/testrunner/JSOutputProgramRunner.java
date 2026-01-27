package dev.argon.testrunner;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.file.PathUtils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class JSOutputProgramRunner implements OutputProgramRunner {
	public JSOutputProgramRunner(RunnerContext context) {
		this.context = context;
	}
	
	private final RunnerContext context;
	
	@Override
	public String runProgram(Path outputDir, List<LibraryOutputInfo> libraryOutputs) throws IOException, InterruptedException, CommandFailureException {
		var runtimeModuleDir = outputDir.resolve("node_modules/@argon-lang/runtime");
		Files.createDirectories(runtimeModuleDir);
		PathUtils.copyDirectory(context.backendsDir().resolve("runtime/js"), runtimeModuleDir);
		
		for(var libraryOutput : libraryOutputs) {
			var libName = LibraryUtils.getLibraryName(libraryOutput.libraryName());
			var libPath = outputDir.resolve("node_modules/@argon-tube", libName);
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
