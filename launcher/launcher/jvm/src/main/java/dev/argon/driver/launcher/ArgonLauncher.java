package dev.argon.driver.launcher;

import dev.argon.backend.api.BackendFactory;
import dev.argon.backend.api.metadata.BackendLoaderOptions;
import dev.argon.backend.api.metadata.BackendMetadata;
import dev.argon.driver.api.BackendMetadataParseException;
import dev.argon.driver.api.CompilerDriver;
import dev.argon.driver.api.CompilerDriverOptions;
import dev.argon.driver.api.command.CompilerDriverOptionValue;
import dev.argon.driver.api.command.CompilerDriverOptionValueAtom;
import dev.argon.driver.api.command.DriverCommand;
import dev.argon.driver.launcher.backendloaders.jsApi.JSBackendFactory;
import dev.argon.esexpr.KeywordMapping;
import dev.argon.vm.api.TubeName;
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
		
		var factories = loadBackendFactories(driver);
		int exitCode;
		try {
			var backendMetadata = factories.stream().map(BackendFactory::metadata).toList();

			var commandStr = driver.parseCommandLineArguments(backendMetadata, args);
			var command = realizeCommandPath(commandStr);

			var options = new CompilerDriverOptions(
				factories,
				command
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
	
	private static List<BackendFactory> loadBackendFactories(CompilerDriver driver) throws IOException {
		var backendsDir = System.getProperty("dev.argon.backends");
		if(backendsDir == null) {
			throw new RuntimeException("No backend directory specified");
		}
		
		List<BackendFactory> factories = new ArrayList<>();
		
		try {
			try(var backendDirStream = Files.list(Path.of(backendsDir))) {
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
	
	private static DriverCommand<Path> realizeCommandPath(DriverCommand<String> command) {
		return switch(command) {
			case DriverCommand.HelpCommand(var isError, var args) ->
				new DriverCommand.HelpCommand<>(isError, args);
			
			case DriverCommand.VersionCommand() ->
				new DriverCommand.VersionCommand<>();
			
			case DriverCommand.ListBackendsCommand() ->
				new DriverCommand.ListBackendsCommand<>();

			case DriverCommand.CompileCommand<String> compileCommand ->
				new DriverCommand.CompileCommand<>(
					compileCommand.tubeName(),
					Path.of(compileCommand.inputDir()),
					Path.of(compileCommand.outputFile()),
					compileCommand.referencedTubes().stream().map(Path::of).toList(),
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

			case DriverCommand.GenIrCommand<String> genIRCommand ->
				new DriverCommand.GenIrCommand<>(
					Path.of(genIRCommand.inputFile()),
					Path.of(genIRCommand.outputFile()),
					genIRCommand.referencedTubes().stream().map(Path::of).toList(),
					genIRCommand.platform()
				);
			
			case DriverCommand.CodegenCommand<String> codegenCommand ->
				new DriverCommand.CodegenCommand<>(
					codegenCommand.backend(),
					Path.of(codegenCommand.inputFile()),
					codegenCommand.referencedTubes().stream().map(Path::of).toList(),
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
								entry -> Path.of(entry.getValue())
							))
					)
				);
		};
	}

	private static CompilerDriverOptionValue<Path> realizeOptionValue(CompilerDriverOptionValue<String> value) {
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

	private static CompilerDriverOptionValueAtom<Path> realizeOptionValueAtom(CompilerDriverOptionValueAtom<String> item) {
		return switch(item) {
			case CompilerDriverOptionValueAtom.Bool(var b) -> new CompilerDriverOptionValueAtom.Bool<>(b);
			case CompilerDriverOptionValueAtom.String(var s) -> new CompilerDriverOptionValueAtom.String<>(s);
			case CompilerDriverOptionValueAtom.Directory(var p) -> new CompilerDriverOptionValueAtom.Directory<>(Path.of(p));
			case CompilerDriverOptionValueAtom.File(var p) -> new CompilerDriverOptionValueAtom.File<>(Path.of(p));
		};
	}

}
