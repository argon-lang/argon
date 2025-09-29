package dev.argon.testrunner;

import com.beust.jcommander.Parameter;

import java.nio.file.Path;
import java.util.List;

class TestRunnerArgs {
    
    @Parameter(required = true)
    public List<Path> testCases;

	@Parameter(names = { "--host" })
	public List<String> hostPlatforms;
    
    @Parameter(names = { "--platform" })
    public List<String> backends;
    
	@Parameter(names = { "--libraries" }, required = true)
	public Path librariesDir;

	@Parameter(names = { "--backends" }, required = true)
	public Path backendsDir;
	
	@Parameter(names = { "--dist" }, required = true)
	public Path distDir;
	
	@Parameter(names = { "--keep" }, description = "Keep temporary files")
	public boolean keepTempFiles = false;
}
