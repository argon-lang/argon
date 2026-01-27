package dev.argon.testrunner;

import dev.argon.driver.api.command.DriverCommand;
import dev.argon.esexpr.KeywordMapping;
import dev.argon.vm.api.TubeName;
import org.apache.commons.io.file.PathUtils;

import java.io.Closeable;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

class TestCaseRunner implements Closeable {

    public TestCaseRunner(RunnerContext context) throws IOException {
        this.context = context;
        tempDir = Files.createTempDirectory("argon-tests");
    }

	private final RunnerContext context;
    private final Path tempDir;
	private boolean keepTempFiles = false;

	
	private final DriverCommandExecutor commandExecutor = new DriverCommandExecutorCLI() {
		@Override
		protected ProcessBuilder createProcessBuilder(ArgumentBuilder argumentBuilder) {
			var pb = new ProcessBuilder();

			List<String> command = new ArrayList<>();
			command.add(context.distDir().resolve("argon").toString());
			argumentBuilder.build(command);
			
			pb.command(command);

			return pb;
		}
	};
	
	
	public void keepTempFiles() {
		keepTempFiles = true;
	}


	private interface TryFunction<T> {
		T apply() throws Exception;
	}

	private static sealed interface Try<T> {
		T get() throws Exception;

		record Success<T>(T value) implements Try<T> {
			@Override
			public T get() throws Exception {
				return value;
			}
		}
		record Failure<T>(Exception exception) implements Try<T> {
			@Override
			public T get() throws Exception {
				throw exception;
			}
		}

		static <T> Try<T> of(TryFunction<T> f) {
			try {
				return new Success<>(f.apply());
			}
			catch(Exception e) {
				return new Failure<>(e);
			}
		}
	}

	
	private final Map<TubeName, Try<Path>> libraryTubes = new ConcurrentHashMap<>();
	private final Map<TubeName, Try<Path>> libraryIR = new ConcurrentHashMap<>();
	private final Map<TubeName, Try<Path>> libraryOutput = new ConcurrentHashMap<>();

    private Path buildLibraryTube(TubeName library) throws Exception {
	    return libraryTubes.computeIfAbsent(library, lib -> Try.of(() -> buildLibraryTubeImpl(lib))).get();
    }

    private Path buildLibraryTubeImpl(TubeName library) throws Exception {
		var libraryName = LibraryUtils.getLibraryName(library);
        var libDir = context.librariesDir().resolve(libraryName);

        var outputLibDir = tempDir.resolve("lib").resolve(context.targetPlatform()).resolve(libraryName);
        Files.createDirectories(outputLibDir);
        var outputFile = outputLibDir.resolve(libraryName + ".artube");
		
		var options = LibraryUtils.platformOptions(library, context.targetPlatform(), libDir);

		var command = new DriverCommand.CompileCommand<String, String, String, String>(
			library,
			libDir.resolve("src").toString(),
			outputFile.toString(),
			List.of(),
			List.of(context.targetPlatform()),
			options
		);
        
        execute(command);
        
        return outputFile;
    }

	private Path buildLibraryIR(TubeName library) throws Exception {
		return libraryIR.computeIfAbsent(library, lib -> Try.of(() -> buildLibraryIRImpl(lib))).get();
	}

	private Path buildLibraryIRImpl(TubeName library) throws Exception {
		var libraryName = LibraryUtils.getLibraryName(library);
		var outputLibDir = tempDir.resolve("lib").resolve(context.targetPlatform()).resolve(libraryName);
		Files.createDirectories(outputLibDir);
		var inputFile = outputLibDir.resolve(libraryName + ".artube");
		var outputFile = outputLibDir.resolve(libraryName + ".arvm");

		var command = new DriverCommand.GenIrCommand<String, String, String, String>(
			inputFile.toString(),
			outputFile.toString(),
			List.of(),
			context.targetPlatform()
		);
		
		execute(command);

		return outputFile;
	}
	private Path buildLibraryOutput(TubeName library) throws Exception {
		return libraryOutput.computeIfAbsent(library, lib -> Try.of(() -> buildLibraryOutputImpl(lib))).get();
	}

	private Path buildLibraryOutputImpl(TubeName library) throws Exception {
		var libraryName = LibraryUtils.getLibraryName(library);
		var outputLibDir = tempDir.resolve("lib").resolve(context.targetPlatform()).resolve(libraryName);
		Files.createDirectories(outputLibDir);
		var outputDir = outputLibDir.resolve("output");
		Files.createDirectories(outputDir);

		var command = new DriverCommand.CodegenCommand<String, String, String, String>(
			context.targetPlatform(),
			
			outputLibDir.resolve(libraryName + ".arvm").toString(),
			List.of(),
			
			new KeywordMapping<>(Map.of()),
			LibraryUtils.outputOptions(context.targetPlatform(), outputDir)
		);
		
		execute(command);

		return outputDir;
	}

	
	public void executeTestCase(GroupedTestCase testCase) throws Exception {
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



		List<String> libraryRefFiles = new ArrayList<>();
		for(var libraryName : testCase.getTestCase().getLibraryTubeNamesOrDefault()) {
			var libTube = buildLibraryTube(libraryName);

			libraryRefFiles.add(libTube.toString());
		}
		
		
		{
			var command = new DriverCommand.CompileCommand<String, String, String, String>(
				new TubeName("Argon", List.of("TestCase")),
				srcDir.toString(),
				tubeFile.toString(),
				libraryRefFiles,
				List.of(context.targetPlatform()),
				new KeywordMapping<>(Map.of())
			);

			try {
				execute(command);
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
			var command = new DriverCommand.GenIrCommand<String, String, String, String>(
				tubeFile.toString(),
				vmirFile.toString(),
				libraryRefFiles,
				context.targetPlatform()
			);
			
			execute(command);
		}
		
		var outputDir = testDir.resolve("output");
		Files.createDirectories(outputDir);

		{
			List<String> libraryIrFiles = new ArrayList<>();
			for(var libraryName : testCase.getTestCase().getLibraryTubeNamesOrDefault()) {
				var libTube = buildLibraryIR(libraryName);
				libraryIrFiles.add(libTube.toString());
			}
			
			var command = new DriverCommand.CodegenCommand<String, String, String, String>(
				context.targetPlatform(),
				vmirFile.toString(),
				libraryIrFiles,
				new KeywordMapping<>(Map.of()),
				LibraryUtils.outputOptions(context.targetPlatform(), outputDir)
			);
			
			execute(command);
		}


		String output;
		{
			var libInfos = new ArrayList<OutputProgramRunner.LibraryOutputInfo>();
			
			for(var libraryName : testCase.getTestCase().getLibraryTubeNamesOrDefault()) {
				var libPath = buildLibraryOutput(libraryName);
				libInfos.add(new OutputProgramRunner.LibraryOutputInfo(libraryName, libPath));
			}
			
			var runner = OutputProgramRunner.forPlatform(context);
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
	
	private void execute(DriverCommand<String, String, String, String> command) throws Exception {
		var output = commandExecutor.execute(command);

		int exitCode = output.exitCode();
		if(exitCode != 0) {
			throw new CommandFailureException("Command completed with exit code " + exitCode + ": " + command, output.output());
		}
	}

    @Override
    public void close() throws IOException {
		if(!keepTempFiles) {
			PathUtils.deleteDirectory(tempDir);	
		}
    }
}
