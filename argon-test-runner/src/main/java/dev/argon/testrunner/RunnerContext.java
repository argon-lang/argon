package dev.argon.testrunner;

import java.nio.file.Path;

public record RunnerContext(
	String hostPlatform,
	String targetPlatform,
	Path librariesDir,
	Path backendsDir,
	Path distDir
) {
}
