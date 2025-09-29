package dev.argon.testrunner;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.file.PathUtils;

import java.io.Closeable;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

class TestCaseRunner implements Closeable {

    public TestCaseRunner(Path libraryDir, Path argonDistDir, TestExecutor executor) throws IOException {
        this.libraryDir = libraryDir;
        this.argonDistDir = argonDistDir;
        this.executor = executor;
        tempDir = Files.createTempDirectory("argon-tests");
    }

    private final Path libraryDir;
    private final Path argonDistDir;
    private final TestExecutor executor;
    private final Path tempDir;
	private boolean keepTempFiles = false;

	
	public void keepTempFiles() {
		keepTempFiles = true;
	}
	
	
	private final Map<LibraryKey, Future<Path>> libraryTubes = new ConcurrentHashMap<>();
	private final Map<LibraryKey, Future<Path>> libraryIR = new ConcurrentHashMap<>();
	private final Map<LibraryKey, Future<Path>> libraryOutput = new ConcurrentHashMap<>();

    private Path buildLibraryTube(LibraryKey library) throws Exception {
        try {
            return libraryTubes.computeIfAbsent(library, lib -> executor.submit(() -> buildLibraryTubeImpl(lib))).get();
        }
        catch(ExecutionException e) {
            if(e.getCause() instanceof Exception e2) {
                throw e2;
            }

            throw e;
        }
    }

    private Path buildLibraryTubeImpl(LibraryKey library) throws Exception {
        var libDir = libraryDir.resolve(library.name());

        var outputLibDir = tempDir.resolve("lib").resolve(library.platform()).resolve(library.name());
        Files.createDirectories(outputLibDir);
        var outputFile = outputLibDir.resolve(library.name() + ".artube");

        var args = new ArrayList<String>();
        args.add("compile");
        args.add("--name");
        args.add(library.name());
        args.add("-i");
        args.add(libDir.resolve("src").toString());
        args.add("-o");
        args.add(outputFile.toString());
		
		args.add("--platform");
		args.add(library.platform());
		
		var options = LibraryPlatformOptions.forLibrary(library, libDir);
		args.addAll(options.arguments());
        
        execute(args);
        
        return outputFile;
    }

	private Path buildLibraryIR(LibraryKey library) throws Exception {
		try {
			return libraryIR.computeIfAbsent(library, lib -> executor.submit(() -> buildLibraryIRImpl(lib))).get();
		}
		catch(ExecutionException e) {
			if(e.getCause() instanceof Exception e2) {
				throw e2;
			}

			throw e;
		}
	}

	private Path buildLibraryIRImpl(LibraryKey library) throws Exception {
		var outputLibDir = tempDir.resolve("lib").resolve(library.platform()).resolve(library.name());
		Files.createDirectories(outputLibDir);
		var inputFile = outputLibDir.resolve(library.name() + ".artube");
		var outputFile = outputLibDir.resolve(library.name() + ".arvm");

		var args = new ArrayList<String>();
		args.add("genir");
		args.add("-i");
		args.add(inputFile.toString());
		args.add("-o");
		args.add(outputFile.toString());

		args.add("--platform");
		args.add(library.platform());

		execute(args);

		return outputFile;
	}
	private Path buildLibraryOutput(LibraryKey library) throws Exception {
		try {
			return libraryOutput.computeIfAbsent(library, lib -> executor.submit(() -> buildLibraryOutputImpl(lib))).get();
		}
		catch(ExecutionException e) {
			if(e.getCause() instanceof Exception e2) {
				throw e2;
			}

			throw e;
		}
	}

	private Path buildLibraryOutputImpl(LibraryKey library) throws Exception {
		var outputLibDir = tempDir.resolve("lib").resolve(library.platform()).resolve(library.name());
		Files.createDirectories(outputLibDir);
		var outputDir = outputLibDir.resolve("output");
		Files.createDirectories(outputDir);

		var args = new ArrayList<String>();
		args.add("codegen");
		args.add(library.platform());
		args.add("-i");
		args.add(outputLibDir.resolve(library.name() + ".arvm").toString());
		args.addAll(getOutputOptions(library.platform(), outputDir));

		execute(args);

		return outputDir;
	}

	
	public void executeTestCase(GroupedTestCase testCase, String platform) throws Exception {
		var testDir = tempDir.resolve("tests");
		for(var part : testCase.getGroup()) {
			testDir = testDir.resolve(part);
		}
		testDir = testDir.resolve(testCase.getBaseName());
		Files.createDirectories(testDir);
		
		var srcDir = testDir.resolve("src");
		Files.createDirectories(srcDir);
		
		for(var inputSource : testCase.getTestCase().getInputSources()) {
			Files.writeString(srcDir.resolve(inputSource.getName()), inputSource.getValue());
		}
		
		var tubeFile = testDir.resolve("Argon.TestCase.artube");

		{
			var args = new ArrayList<String>();
			args.add("compile");
			args.add("--name");
			args.add("Argon.TestCase");
			args.add("-i");
			args.add(srcDir.toString());
			args.add("-o");
			args.add(tubeFile.toString());

			for(var libraryName : testCase.getTestCase().getLibrariesOrDefault()) {
				var library = new LibraryKey(libraryName, platform);
				var libTube = buildLibraryTube(library);
				
				args.add("-r");
				args.add(libTube.toString());
			}
			
			args.add("--platform");
			args.add(platform);

			try {
				execute(args);
			}
			catch(CommandFailureException e) {
				var expectedError = testCase.getTestCase().getExpectedError();
				if(expectedError != null && e.getCompilerOutput().contains(expectedError)) {
					return;
				}
				
				throw e;
			}
		}

		var vmirFile = testDir.resolve("Argon.TestCase.arvm");
		{
			var args = new ArrayList<String>();
			args.add("genir");
			args.add("-i");
			args.add(tubeFile.toString());
			args.add("-o");
			args.add(vmirFile.toString());
			
			for(var libraryName : testCase.getTestCase().getLibrariesOrDefault()) {
				var library = new LibraryKey(libraryName, platform);
				var libTube = buildLibraryTube(library);

				args.add("-r");
				args.add(libTube.toString());
			}

			args.add("--platform");
			args.add(platform);
			
			execute(args);
		}
		
		var outputDir = testDir.resolve("output");
		Files.createDirectories(outputDir);

		{
			var args = new ArrayList<String>();
			args.add("codegen");
			args.add(platform);
			args.add("-i");
			args.add(vmirFile.toString());
			
			for(var libraryName : testCase.getTestCase().getLibrariesOrDefault()) {
				var library = new LibraryKey(libraryName, platform);
				var libIR = buildLibraryIR(library);

				args.add("-r");
				args.add(libIR.toString());
			}
			
			args.addAll(getOutputOptions(platform, outputDir));
			
			execute(args);
		}


		String output;
		{
			var libInfos = new ArrayList<OutputProgramRunner.LibraryOutputInfo>();
			
			for(var libraryName : testCase.getTestCase().getLibrariesOrDefault()) {
				var libPath = buildLibraryOutput(new LibraryKey(libraryName, platform));
				libInfos.add(new OutputProgramRunner.LibraryOutputInfo(libraryName, libPath));
			}
			
			var runner = OutputProgramRunner.forPlatform(platform);
			output = runner.runProgram(outputDir, libInfos);
		}
		
		var expectedOutput = testCase.getTestCase().getExpectedOutput();
		if(expectedOutput != null) {
			if(!normalize(expectedOutput).equals(normalize(output))) {
				throw new Exception("Output does not match\nExpected:\n" + expectedOutput + "\nActual:" + output);
			}
			
			return;
		}
		
		throw new Exception("Expected error did not occur");
	}
	
	private String normalize(String s) {
		return Arrays.stream(s.trim().split("\\n"))
			.map(String::trim)
			.collect(Collectors.joining());
	}
	

	private List<String> getOutputOptions(String platform, Path outputDir) throws IOException {
		return switch(platform) {
			case "js" -> List.of(
				"--js-modules",
				outputDir.toString(),
				"--js-package-json",
				outputDir.resolve("package.json").toString()
			);
			default -> throw new IllegalArgumentException("Unknown platform: " + platform);
		};
	}
	
	private void execute(List<String> args) throws Exception {
		var pb = new ProcessBuilder();

		var command = new ArrayList<String>(args.size() + 1);
		command.add(argonDistDir.resolve("argon").toString());
		command.addAll(args);
		pb.command(command);

		pb.redirectErrorStream(true);
		pb.redirectOutput(ProcessBuilder.Redirect.PIPE);
		
		var process = pb.start();
		
		String output;
		try(var reader = process.getInputStream()) {
			output = IOUtils.toString(reader, StandardCharsets.UTF_8);
		}
		
		int exitCode = process.waitFor();
		if(exitCode != 0) {
			throw new CommandFailureException("Command completed with exit code " + exitCode + ": " + command, output);
		}
	}

    @Override
    public void close() throws IOException {
		if(!keepTempFiles) {
			PathUtils.deleteDirectory(tempDir);	
		}
    }
}
