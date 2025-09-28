package dev.argon.testrunner;

import com.google.common.collect.ImmutableList;

import java.nio.file.Path;

record LibraryPlatformOptions(
    ImmutableList<String> arguments
) {
	
	public static LibraryPlatformOptions forLibrary(LibraryKey library, Path libraryDir) {
		return switch(library.name()) {
			case "Argon.Core" -> switch(library.platform()) {
				case "js" -> new LibraryPlatformOptions(ImmutableList.of("--js-externs", libraryDir.resolve("js/externs.js").toString()));
				default -> throw new IllegalArgumentException("Unknown platform: " + library.platform());
			};
			
			default -> throw new IllegalArgumentException("Unknown library: " + library);
		};
	}
	
}
