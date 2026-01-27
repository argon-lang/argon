package dev.argon.testrunner;

import java.nio.file.Path;

public record RunnerContext(
	HostPlatform hostPlatform,
	Backend targetPlatform,
	Path librariesDir,
	Path backendsDir,
	Path distDir
) {
}
