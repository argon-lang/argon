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

public class TestCaseRunner implements Closeable {

    public TestCaseRunner(RunnerContext context, DriverCommandExecutor commandExecutor, OutputProgramRunner runner) throws IOException {
        this.context = context;
        this.commandExecutor = commandExecutor;
        this.runner = runner;
        tempDir = Files.createTempDirectory("argon-tests");
    }

	private final RunnerContext context;
    private final Path tempDir;

	
	private final DriverCommandExecutor commandExecutor;
	private final OutputProgramRunner runner;


	private interface TryFunction<T> {
		T apply() throws Throwable;
	}

	private static sealed interface Try<T> {
		T get() throws Throwable;

		record Success<T>(T value) implements Try<T> {
			@Override
			public T get() throws Throwable {
				return value;
			}
		}
		record Failure<T>(Throwable exception) implements Try<T> {
			@Override
			public T get() throws Throwable {
				throw exception;
			}
		}

		static <T> Try<T> of(TryFunction<T> f) {
			try {
				return new Success<>(f.apply());
			}
			catch(Throwable e) {
				return new Failure<>(e);
			}
		}
	}

	
	private final Map<TubeName, Try<Path>> libraryTubes = new ConcurrentHashMap<>();
	private final Map<TubeName, Try<Path>> libraryIR = new ConcurrentHashMap<>();
	private final Map<TubeName, Try<Path>> libraryOutput = new ConcurrentHashMap<>();

    private Path buildLibraryTube(TubeName library) throws Throwable {
	    return libraryTubes.computeIfAbsent(library, lib -> Try.of(() -> buildLibraryTubeImpl(lib))).get();
    }

    private Path buildLibraryTubeImpl(TubeName library) throws Throwable {
		var libraryName = LibraryUtils.getLibraryName(library);
        var libDir = context.librariesDir().resolve(libraryName);

        var outputLibDir = tempDir.resolve("lib").resolve(context.targetPlatform().backendId()).resolve(libraryName);
        Files.createDirectories(outputLibDir);
        var outputFile = outputLibDir.resolve(libraryName + ".artube");
		
		var options = LibraryUtils.platformOptions(library, context.targetPlatform(), libDir);

		var command = new DriverCommand.CompileCommand<String, String, String, String>(
			library,
			libDir.resolve("src").toString(),
			outputFile.toString(),
			List.of(),
			List.of(context.targetPlatform().backendId()),
			options
		);
        
        execute(command);
        
        return outputFile;
    }

	private Path buildLibraryIR(TubeName library) throws Throwable {
		return libraryIR.computeIfAbsent(library, lib -> Try.of(() -> buildLibraryIRImpl(lib))).get();
	}

	private Path buildLibraryIRImpl(TubeName library) throws Throwable {
		var libraryName = LibraryUtils.getLibraryName(library);
		var outputLibDir = tempDir.resolve("lib").resolve(context.targetPlatform().backendId()).resolve(libraryName);
		Files.createDirectories(outputLibDir);
		var inputFile = outputLibDir.resolve(libraryName + ".artube");
		var outputFile = outputLibDir.resolve(libraryName + ".arvm");

		var command = new DriverCommand.GenIrCommand<String, String, String, String>(
			inputFile.toString(),
			outputFile.toString(),
			List.of(),
			context.targetPlatform().backendId()
		);
		
		execute(command);

		return outputFile;
	}
	private Path buildLibraryOutput(TubeName library) throws Throwable {
		return libraryOutput.computeIfAbsent(library, lib -> Try.of(() -> buildLibraryOutputImpl(lib))).get();
	}

	private Path buildLibraryOutputImpl(TubeName library) throws Throwable {
		var libraryName = LibraryUtils.getLibraryName(library);
		var outputLibDir = tempDir.resolve("lib").resolve(context.targetPlatform().backendId()).resolve(libraryName);
		Files.createDirectories(outputLibDir);
		var outputDir = outputLibDir.resolve("output");
		Files.createDirectories(outputDir);

		var command = new DriverCommand.CodegenCommand<String, String, String, String>(
			context.targetPlatform().backendId(),
			
			outputLibDir.resolve(libraryName + ".arvm").toString(),
			List.of(),
			
			new KeywordMapping<>(Map.of()),
			LibraryUtils.outputOptions(context.targetPlatform(), outputDir)
		);
		
		execute(command);

		return outputDir;
	}

	
	public TestResult executeTestCase(GroupedTestCase testCase) throws Throwable {
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
				List.of(context.targetPlatform().backendId()),
				new KeywordMapping<>(Map.of())
			);

			try {
				execute(command);
			}
			catch(CommandFailureException e) {
				return new TestResult.CompileError(e.getCompilerOutput());
			}
		}

		var vmirFile = testDir.resolve("Argon.TestCase.arvm");
		{
			var command = new DriverCommand.GenIrCommand<String, String, String, String>(
				tubeFile.toString(),
				vmirFile.toString(),
				libraryRefFiles,
				context.targetPlatform().backendId()
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
				context.targetPlatform().backendId(),
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
			
			output = runner.runProgram(outputDir, libInfos);
		}
		
		return new TestResult.Executed(output);
	}
	
	private void execute(DriverCommand<String, String, String, String> command) throws Throwable {
		var output = commandExecutor.execute(command);

		int exitCode = output.exitCode();
		if(exitCode != 0) {
			throw new CommandFailureException("Command completed with exit code " + exitCode + ": " + command + "\n" + output.output(), output.output());
		}
	}

    @Override
    public void close() throws IOException {
	    PathUtils.deleteDirectory(tempDir);
    }
}
