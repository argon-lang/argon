package dev.argon.driver.api;

import java.io.*;
import java.nio.file.Path;
import java.util.List;

import dev.argon.backend.api.*;
import dev.argon.driver.api.command.DriverCommand;

public final class CompilerDriverOptions {
    public CompilerDriverOptions(
		List<BackendFactory> backendFactories,
		DriverCommand<
			BinaryResource<IOException>,
			DirectoryResource<IOException>,
			BinaryResourceSink<IOException>,
			DirectoryResourceSink<IOException>
		> command,
		InputStream stdin,
		PrintStream stdout,
		PrintStream stderr
    ) {
        this.backendFactories = backendFactories;
        this.command = command;
		
	    this.stdin = stdin;
	    this.stdout = stdout;
	    this.stderr = stderr;
    }
    
    private final List<BackendFactory> backendFactories;
    private final DriverCommand<
	    BinaryResource<IOException>,
	    DirectoryResource<IOException>,
	    BinaryResourceSink<IOException>,
	    DirectoryResourceSink<IOException>
    > command;
	private final InputStream stdin;
	private final PrintStream stdout;
	private final PrintStream stderr;

	public List<BackendFactory> getBackendFactories() {
		return backendFactories;
	}
	
    public DriverCommand<
	    BinaryResource<IOException>,
	    DirectoryResource<IOException>,
	    BinaryResourceSink<IOException>,
	    DirectoryResourceSink<IOException>
    > getCommand() {
        return command;
    }
	
	public InputStream getStdin() {
		return stdin;
	}
	
	public PrintStream getStdout() {
		return stdout;
	}
	
	public PrintStream getStderr() {
		return stderr;
	}
    
}
