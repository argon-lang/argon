package dev.argon.testrunner;

import com.google.common.collect.ImmutableList;

import java.nio.file.Path;

record LibraryPlatformOptions(
    ImmutableList<String> arguments
) {
	
	public static LibraryPlatformOptions forLibrary(String library, String platform, Path libraryDir) {
		return switch(library) {
			case "Argon.Core" -> switch(platform) {
				case "js" -> new LibraryPlatformOptions(ImmutableList.of("--js-externs", libraryDir.resolve("js/externs.js").toString()));
				default -> throw new IllegalArgumentException("Unknown platform: " + library);
			};
			
			default -> throw new IllegalArgumentException("Unknown library: " + library);
		};
	}
	
}
