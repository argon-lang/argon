package dev.argon.testrunner;

import com.beust.jcommander.*;

import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

class TestRunnerArgs {
	public static final List<HostPlatform> ALL_HOST_PLATFORMS = List.of(HostPlatform.values());
	public static final List<Backend> ALL_BACKENDS = List.of(Backend.values());
    
	@Parameter(names = { "-h", "--help" }, help = true)
	public boolean help = false;
	
    @Parameter(required = true)
    public List<Path> testCases;

	@Parameter(names = { "--host" }, validateValueWith = HostValueValidator.class)
	public List<HostPlatform> hostPlatforms = ALL_HOST_PLATFORMS;
    
    @Parameter(names = { "--platform" }, validateValueWith = BackendValueValidator.class)
    public List<Backend> backends = ALL_BACKENDS;
    
	@Parameter(names = { "--libraries" }, required = true)
	public Path librariesDir;

	@Parameter(names = { "--backends" }, required = true)
	public Path backendsDir;
	
	@Parameter(names = { "--dist" }, required = true)
	public Path distDir;
	
	@Parameter(names = { "--keep" }, description = "Keep temporary files")
	public boolean keepTempFiles = false;

	public sealed static abstract class PlatformValueValidatorBase<A> implements IValueValidator<List<A>> {

		protected abstract List<A> getPlatforms();
		protected abstract String getPlatformMessage();

		@Override
		public void validate(String name, List<A> value) throws ParameterException {
			Set<A> platforms = new HashSet<>();
			for(var platform : value) {
				if(!platforms.add(platform)) {
					throw new ParameterException("Duplicate " + getPlatformMessage() + ": " + platform);
				}
			}
		}
	}

	public final static class HostValueValidator extends PlatformValueValidatorBase<HostPlatform> {
		@Override
		protected List<HostPlatform> getPlatforms() {
			return ALL_HOST_PLATFORMS;
		}

		@Override
		protected String getPlatformMessage() {
			return "host platform";
		}
	}

	public final static class BackendValueValidator extends PlatformValueValidatorBase<Backend> {
		@Override
		protected List<Backend> getPlatforms() {
			return ALL_BACKENDS;
		}

		@Override
		protected String getPlatformMessage() {
			return "backend";
		}
	}
}
