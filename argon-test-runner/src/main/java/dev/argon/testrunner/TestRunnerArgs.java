package dev.argon.testrunner;

import com.beust.jcommander.*;

import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

class TestRunnerArgs {
	public static final List<String> ALL_HOST_PLATFORMS = List.of("jvm", "js");
	public static final List<String> ALL_BACKENDS = List.of("js");
    
	@Parameter(names = { "-h", "--help" }, help = true)
	public boolean help = false;
	
    @Parameter(required = true)
    public List<Path> testCases;

	@Parameter(names = { "--host" }, validateValueWith = HostValueValidator.class)
	public List<String> hostPlatforms = ALL_HOST_PLATFORMS;
    
    @Parameter(names = { "--platform" }, validateValueWith = BackendValueValidator.class)
    public List<String> backends = ALL_BACKENDS;
    
	@Parameter(names = { "--libraries" }, required = true)
	public Path librariesDir;

	@Parameter(names = { "--backends" }, required = true)
	public Path backendsDir;
	
	@Parameter(names = { "--dist" }, required = true)
	public Path distDir;
	
	@Parameter(names = { "--keep" }, description = "Keep temporary files")
	public boolean keepTempFiles = false;

	public sealed static abstract class PlatformValueValidatorBase implements IValueValidator<List<String>> {

		protected abstract List<String> getPlatforms();
		protected abstract String getPlatformMessage();

		@Override
		public void validate(String name, List<String> value) throws ParameterException {
			Set<String> platforms = new HashSet<>();
			for(var platform : value) {
				if(!platforms.add(platform)) {
					throw new ParameterException("Duplicate " + getPlatformMessage() + ": " + platform);
				}

				if(!getPlatforms().contains(platform)) {
					throw new ParameterException("Unsupported " + getPlatformMessage() + ": " + platform);
				}
			}
		}
	}

	public final static class HostValueValidator extends PlatformValueValidatorBase {
		@Override
		protected List<String> getPlatforms() {
			return ALL_HOST_PLATFORMS;
		}

		@Override
		protected String getPlatformMessage() {
			return "host platform";
		}
	}

	public final static class BackendValueValidator extends PlatformValueValidatorBase {
		@Override
		protected List<String> getPlatforms() {
			return ALL_BACKENDS;
		}

		@Override
		protected String getPlatformMessage() {
			return "host platform";
		}
	}
}
