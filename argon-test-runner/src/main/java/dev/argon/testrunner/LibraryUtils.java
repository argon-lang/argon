package dev.argon.testrunner;

import dev.argon.driver.api.command.CompilerDriverOptionValue;
import dev.argon.driver.api.command.CompilerDriverOptionValueAtom;
import dev.argon.driver.api.command.CompilerDriverOutput;
import dev.argon.esexpr.KeywordMapping;
import dev.argon.vm.api.TubeName;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

public class LibraryUtils {
	
	public static String getLibraryName(TubeName tubeName) {
		var name = new StringBuilder();
		name.append(tubeName.head());
		for(var segment : tubeName.tail()) {
			name.append('.');
			name.append(segment);
		}
		return name.toString();
	}
	
	public static TubeName parseLibraryName(String name) {
		var segments = name.split("\\.");
		return new TubeName(segments[0], List.of(segments).subList(1, segments.length));
	}

	public static final TubeName LIBRARY_ARGON_CORE = new TubeName("Argon", List.of("Core"));
	

	public static KeywordMapping<KeywordMapping<CompilerDriverOptionValue<String, String>>> platformOptions(TubeName library, Backend platform, Path libraryDir) {
		Map<String, CompilerDriverOptionValue<String, String>> optionMap;
		if(library.equals(LIBRARY_ARGON_CORE)) {
			optionMap = switch(platform) {
				case JS -> Map.of(
					"externs", new CompilerDriverOptionValue.Many<>(
						new CompilerDriverOptionValueAtom.File<>(libraryDir.resolve("js/externs.js").toString()),
						List.of()
					)
				);
			};
		}
		else {
			throw new IllegalArgumentException("Unknown library: " + library);
		}
		
		return new KeywordMapping<>(Map.of(platform.backendId(), new KeywordMapping<>(optionMap)));
	}


	public static KeywordMapping<CompilerDriverOutput<String, String>> outputOptions(Backend platform, Path outputDir) throws IOException {
		Map<String, CompilerDriverOutput<String, String>> optionMap = switch(platform) {
			case JS -> Map.of(
				"modules",
				new CompilerDriverOutput.Directory<>(outputDir.toString()),
				"package-json",
				new CompilerDriverOutput.File<>(outputDir.resolve("package.json").toString())
			);
		};
		
		return new KeywordMapping<>(optionMap);
	}
	
}
