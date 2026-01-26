package dev.argon.driver.api;

import java.io.IOException;
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
		> command
    ) {
        this.backendFactories = backendFactories;
        this.command = command;
    }
    
    private final List<BackendFactory> backendFactories;
    private final DriverCommand<
	    BinaryResource<IOException>,
	    DirectoryResource<IOException>,
	    BinaryResourceSink<IOException>,
	    DirectoryResourceSink<IOException>
    > command;
    
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
    
}
