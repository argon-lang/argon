package dev.argon.driver.launcher;

import dev.argon.backend.api.*;
import dev.argon.backend.api.metadata.BackendLoaderOptions;
import dev.argon.backend.api.metadata.BackendMetadata;
import dev.argon.driver.api.BackendMetadataParseException;
import dev.argon.driver.api.CompilerDriver;
import dev.argon.driver.api.CompilerDriverOptions;
import dev.argon.driver.api.command.CompilerDriverOptionValue;
import dev.argon.driver.api.command.CompilerDriverOptionValueAtom;
import dev.argon.driver.api.command.CompilerDriverOutput;
import dev.argon.driver.api.command.DriverCommand;
import dev.argon.driver.launcher.backendloaders.jsApi.JSBackendFactory;
import dev.argon.esexpr.KeywordMapping;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public final class ArgonLauncher {
    private ArgonLauncher() { }
    
    
    public static void main(String[] args) throws Throwable {
		CompilerDriver driver = new dev.argon.driver.CompilerDriver();
		
		var backendsDirStr = System.getProperty("dev.argon.backends");
		if(backendsDirStr == null) {
			System.err.println("No backend directory specified");
			System.exit(1);
		}
		
		var backendsDir = Path.of(backendsDirStr);
		
		var factories = loadBackendFactories(driver, backendsDir);
		int exitCode;
		try {
			
			var backendMetadata = factories.stream().map(BackendFactory::metadata).toList();

			var commandStr = driver.parseCommandLineArguments(backendMetadata, args);
			var command = realizeCommandPath(commandStr);

			var options = new CompilerDriverOptions(
				factories,
				command,
				System.in,
				System.out,
				System.err
			);
			
			exitCode = driver.runCommand(options);
		}
		finally {
			for(var factory : factories) {
				factory.close();
			}
		}
		
	    System.exit(exitCode);
    }
	
	public static List<BackendFactory> loadBackendFactories(CompilerDriver driver, Path backendsDir) throws IOException {
		List<BackendFactory> factories = new ArrayList<>();
		
		try {
			try(var backendDirStream = Files.list(backendsDir)) {
				for(Iterator<@NotNull Path> it = backendDirStream.iterator(); it.hasNext(); ) {
					var backendDir = it.next();
					
					var metadataPath = backendDir.resolve("backend.toml");
					if(!Files.exists(metadataPath)) {
						continue;
					}
					
					var metadata = loadMetadata(driver, metadataPath);
					
					var factory = loadBackendFactory(backendDir, metadata);
					if(factory != null) {
						factories.add(factory);
					}
				}
			}
		}
		catch(UncheckedIOException e) {
			if(e.getCause() instanceof IOException ioe) {
				throw ioe;
			}
			
			throw e;
		}
		
		return factories;
	}

	private static BackendMetadata loadMetadata(CompilerDriver driver, Path path) {
		String content;
		try {
			content = Files.readString(path);
		}
		catch(IOException e) {
			throw new RuntimeException("Failed to read backend metadata file: " + path, e);
		}
		
		try {
			return driver.parseMetadata(content);
		}
		catch(BackendMetadataParseException e) {
			throw new RuntimeException("Failed to parse backend metadata file: " + path, e);
		}
	}

	private static BackendFactory loadBackendFactory(Path backendDir, BackendMetadata metadata) {
		for(var loader : metadata.loaders()) {
			switch(loader) {
				case BackendLoaderOptions.Js(var jsLoaderOptions) -> {
					return new JSBackendFactory(backendDir, metadata, jsLoaderOptions);
				}
			}
		}
		
		return null;
	}
	
	public static DriverCommand<BinaryResource<IOException>, DirectoryResource<IOException>, BinaryResourceSink<IOException>, DirectoryResourceSink<IOException>> realizeCommandPath(DriverCommand<String, String, String, String> command) {
		return switch(command) {
			case DriverCommand.HelpCommand(var isError, var args) ->
				new DriverCommand.HelpCommand<>(isError, args);
			
			case DriverCommand.VersionCommand() ->
				new DriverCommand.VersionCommand<>();
			
			case DriverCommand.ListBackendsCommand() ->
				new DriverCommand.ListBackendsCommand<>();

			case DriverCommand.CompileCommand<String, String, String, String> compileCommand ->
				new DriverCommand.CompileCommand<>(
					compileCommand.tubeName(),
					new PathDirectoryResource(Path.of(compileCommand.inputDir())),
					new PathBinaryResourceSink(Path.of(compileCommand.outputFile())),
					compileCommand.referencedTubes().stream().<BinaryResource<IOException>>map(p -> new PathBinaryResource(Path.of(p))).toList(),
					compileCommand.supportedPlatforms(),
					new KeywordMapping<>(
						compileCommand.platformOptions()
							.map()
							.entrySet()
							.stream()
							.collect(Collectors.toUnmodifiableMap(
								Map.Entry::getKey,
								entry -> new KeywordMapping<>(
									entry.getValue()
										.map()
										.entrySet()
										.stream()
										.collect(Collectors.toUnmodifiableMap(
											Map.Entry::getKey,
											entry2 -> realizeOptionValue(entry2.getValue())
										))
								)
							))
					)
				);

			case DriverCommand.GenIrCommand<String, String, String, String> genIRCommand ->
				new DriverCommand.GenIrCommand<>(
					new PathBinaryResource(Path.of(genIRCommand.inputFile())),
					new PathBinaryResourceSink(Path.of(genIRCommand.outputFile())),
					genIRCommand.referencedTubes().stream().<BinaryResource<IOException>>map(p -> new PathBinaryResource(Path.of(p))).toList(),
					genIRCommand.platform()
				);
			
			case DriverCommand.CodegenCommand<String, String, String, String> codegenCommand ->
				new DriverCommand.CodegenCommand<>(
					codegenCommand.backend(),
					new PathBinaryResource(Path.of(codegenCommand.inputFile())),
					codegenCommand.referencedTubes().stream().<BinaryResource<IOException>>map(p -> new PathBinaryResource(Path.of(p))).toList(),
					new KeywordMapping<>(
						codegenCommand.platformOptions()
							.map()
							.entrySet()
							.stream()
							.collect(Collectors.toUnmodifiableMap(
								Map.Entry::getKey,
								entry -> realizeOptionValue(entry.getValue())
							))
					),
					new KeywordMapping<>(
						codegenCommand.platformOutputOptions()
							.map()
							.entrySet()
							.stream()
							.collect(Collectors.toUnmodifiableMap(
								Map.Entry::getKey,
								entry -> realizeOutputValue(entry.getValue())
							))
					)
				);
		};
	}

	private static CompilerDriverOptionValue<BinaryResource<IOException>, DirectoryResource<IOException>> realizeOptionValue(CompilerDriverOptionValue<String, String> value) {
		return switch(value) {
			case CompilerDriverOptionValue.Many(var head, var tail) ->
				new CompilerDriverOptionValue.Many<>(
					ArgonLauncher.realizeOptionValueAtom(head),
					tail.stream().map(ArgonLauncher::realizeOptionValueAtom).toList()
				);
			case CompilerDriverOptionValue.Single(var item) ->
				new CompilerDriverOptionValue.Single<>(realizeOptionValueAtom(item));
		};
	}

	private static CompilerDriverOptionValueAtom<BinaryResource<IOException>, DirectoryResource<IOException>> realizeOptionValueAtom(CompilerDriverOptionValueAtom<String, String> item) {
		return switch(item) {
			case CompilerDriverOptionValueAtom.Bool(var b) -> new CompilerDriverOptionValueAtom.Bool<>(b);
			case CompilerDriverOptionValueAtom.String(var s) -> new CompilerDriverOptionValueAtom.String<>(s);
			case CompilerDriverOptionValueAtom.Directory(var p) -> new CompilerDriverOptionValueAtom.Directory<>(new PathDirectoryResource(Path.of(p)));
			case CompilerDriverOptionValueAtom.File(var p) -> new CompilerDriverOptionValueAtom.File<>(new PathBinaryResource(Path.of(p)));
		};
	}
	
	private static CompilerDriverOutput<BinaryResourceSink<IOException>, DirectoryResourceSink<IOException>> realizeOutputValue(CompilerDriverOutput<String, String> output) {
		return switch(output) {
			case CompilerDriverOutput.File(var f) ->
				new CompilerDriverOutput.File<>(new PathBinaryResourceSink(Path.of(f)));
				
			case CompilerDriverOutput.Directory(var dir) ->
				new CompilerDriverOutput.Directory<>(new PathDirectoryResourceSink(Path.of(dir)));
		};
	}

}
