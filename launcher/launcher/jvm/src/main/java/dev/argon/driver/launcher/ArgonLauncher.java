package dev.argon.driver.launcher;

import dev.argon.backend.api.BackendFactory;
import dev.argon.backend.api.metadata.BackendLoaderOptions;
import dev.argon.backend.api.metadata.BackendMetadata;
import dev.argon.driver.api.BackendMetadataParseException;
import dev.argon.driver.api.CompilerDriver;
import dev.argon.driver.api.CompilerDriverOptions;
import dev.argon.driver.launcher.backendloaders.jsApi.JSBackendFactory;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public final class ArgonLauncher {
    private ArgonLauncher() { }
    
    
    public static void main(String[] args) throws Throwable {
		CompilerDriver driver = new dev.argon.driver.CompilerDriver();
		
		var factories = loadBackendFactories(driver);
		int exitCode;
		try {
			var backendMetadata = factories.stream().map(BackendFactory::metadata).toList();

			var command = driver.parseCommandLineArguments(backendMetadata, args);

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

}
